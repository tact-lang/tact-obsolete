module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Caml.Format
    module Lang = Lang.Make (Config)
    module Interpreter = Interpreter.Make (Config)

    let format_to_string : 'a. 'a -> (formatter -> 'a -> unit) -> string =
     fun x show ->
      let buffer = Buffer.create 0 in
      let f = Format.formatter_of_buffer buffer in
      show f x ; pp_print_flush f () ; Buffer.contents buffer

    type error = [Lang.error | Interpreter.error]

    let list_iter ~f ~flast l =
      let open Base in
      match (List.drop_last l, List.last l) with
      | Some rest, Some last ->
          List.iter rest ~f ; flast last
      | _ ->
          ()

    let rec pp_expr : _ -> Lang.expr -> _ =
     fun f ex ->
      match ex.value with
      | Value v ->
          pp_value f v
      | FunctionCall (fname, args, is_ty) ->
          pp_expr f fname ;
          pp_print_string f (if is_ty then "[" else "(") ;
          list_iter args
            ~f:(fun e -> pp_expr f e ; pp_print_string f ", ")
            ~flast:(fun e -> pp_expr f e) ;
          pp_print_string f (if is_ty then "]" else ")")
      | Reference (name, _) | ResolvedReference (name, _) ->
          pp_print_string f name.value
      | StructField (s, field, _) ->
          pp_expr f s ;
          pp_print_string f "." ;
          pp_print_string f field.value
      | _ ->
          pp_print_string f "<anonymous>"

    and pp_value f = function
      | Integer i ->
          Z.pp_print f i
      | Builtin b ->
          pp_print_string f b
      | Type t ->
          pp_type f t
      | Bool true ->
          pp_print_string f "true"
      | Bool false ->
          pp_print_string f "false"
      | String s ->
          pp_print_string f {|"|} ;
          pp_print_string f s ;
          pp_print_string f {|"|}
      | _ ->
          pp_print_string f "<anonymous>"

    and pp_type f = function
      | TypeN 0 ->
          pp_print_string f "Type"
      | TypeN n ->
          pp_print_string f "Type" ; pp_print_int f n
      | IntegerType ->
          pp_print_string f "Integer"
      | BoolType ->
          pp_print_string f "Bool"
      | VoidType ->
          pp_print_string f "VoidType"
      | BuiltinType t ->
          pp_print_string f t
      | StructType s ->
          pp_print_string f "<struct " ;
          pp_print_int f s ;
          pp_print_string f ">"
      | _ ->
          pp_print_string f "<anonymous>"

    module DiagnosticMsg = struct
      open Config
      open Located.Lexing'

      type t =
        { severity : [`Error | `Warn];
          diagnostic_id : int;
          diagnostic_msg : string;
          spans : (span * string) list;
          additional_msg : (string * string) list }

      open struct
        (*
        Notes about position type.
        pos_cnum - offset from the start of the file.
        pos_lnum - сurrent line.
        pos_bol - offset from the start of the file to the start of the line.
          
      *)
        let string_repeat s n =
          let s = Bytes.of_string s in
          let len = Bytes.length s in
          let res = Bytes.create (n * len) in
          for i = 0 to Int.pred n do
            Bytes.blit s 0 res (i * len) len
          done ;
          Bytes.to_string res

        let show_start_line f e =
          pp_print_string f
            (match e.severity with `Error -> "Error" | `Warn -> "Warn") ;
          pp_print_string f "[" ;
          pp_print_int f e.diagnostic_id ;
          pp_print_string f "]: " ;
          pp_print_string f e.diagnostic_msg

        let show_empty_line_no_newline f line_num_size spaces_count =
          pp_print_string f (string_repeat " " line_num_size) ;
          pp_print_string f "|" ;
          pp_print_string f (string_repeat " " spaces_count)

        let is_dummy (pos1, pos2) =
          equal_position pos1 dummy_pos || equal_position pos2 dummy_pos

        let range ((pos1, _) as range) =
          if is_dummy range then sprintf "File: <unknown>\n"
          else
            let file = pos1.pos_fname in
            let line = pos1.pos_lnum in
            let char1 = pos1.pos_cnum - pos1.pos_bol in
            (* yes, [pos1.pos_bol] *)
            sprintf "File: \"%s\":%d:%d" file line char1

        let extract_code_line text (pos1, pos2) =
          let end_pos =
            let quit_loop = ref false in
            let pos_newline = ref pos1.pos_cnum in
            while not !quit_loop do
              try
                if
                  Char.equal (String.get text !pos_newline) '\n'
                  || Char.equal (String.get text !pos_newline) '\r'
                then quit_loop := true
                else pos_newline := !pos_newline + 1
              with _ -> quit_loop := true
            done ;
            !pos_newline
          in
          let hint_len =
            match Int.equal pos1.pos_lnum pos2.pos_lnum with
            | true ->
                pos2.pos_cnum - pos1.pos_cnum
            | false ->
                end_pos - pos1.pos_cnum + 3
          in
          (String.sub text pos1.pos_bol (end_pos - pos1.pos_bol), hint_len)

        let show_place_one_span f ((pos1, pos2) as span) span_str code
            line_num_size =
          (* Line 1 *)
          show_empty_line_no_newline f line_num_size 0 ;
          pp_print_newline f () ;
          (* Line 2 *)
          pp_print_int f pos1.pos_lnum ;
          pp_print_string f " | " ;
          let code_line, len = extract_code_line code span in
          let code_line =
            match pos1.pos_lnum == pos2.pos_lnum with
            | true ->
                code_line
            | false ->
                code_line ^ "..."
          in
          pp_print_string f code_line ;
          pp_print_newline f () ;
          let offset = pos1.pos_cnum - pos1.pos_bol in
          (* Line 3 *)
          show_empty_line_no_newline f line_num_size (offset + 1) ;
          pp_print_string f (string_repeat "^" len) ;
          pp_print_string f " " ;
          pp_print_string f span_str ;
          pp_print_newline f () ;
          ()

        (* This functions works correctly only with numbers bigger than 0 *)
        let int_digits_count i =
          let rec int_digits_count_inner c = function
            | 0 ->
                c
            | n ->
                int_digits_count_inner (c + 1) (n / 10)
          in
          int_digits_count_inner 0 i
      end

      let show f e code =
        let open Base in
        let ((pos1, _) as span1), _ =
          List.hd_exn e.spans |> fun (s, e) -> (span_to_concrete s, e)
        in
        show_start_line f e ;
        pp_print_newline f () ;
        pp_print_string f (range span1) ;
        pp_print_newline f () ;
        if equal_position pos1 dummy_pos then ()
        else
          match e.spans with
          | (s, sm) :: [] ->
              let line_num_size = int_digits_count pos1.pos_lnum + 1 in
              show_place_one_span f (span_to_concrete s) sm code line_num_size
          | _ ->
              Errors.ice "There are should be only one span in the list"
    end

    let show_error : string -> error -> string =
     fun code e ->
      format_to_string ()
      @@ fun f _ ->
      match e with
      | `DuplicateField (field, _) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Duplicate struct field " ^ field.value;
              spans = [(field.span, "Duplicated")];
              additional_msg = [] }
            code
      | `DuplicateVariant (ty, span) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Duplicate variant with type " ^ format_to_string ty pp_type;
              spans = [(span, "Duplicated variant in this union")];
              additional_msg = [] }
            code
      | `UnresolvedIdentifier id ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Unresolved identifier " ^ id.value;
              spans = [(id.span, "Cannot resolve this identifier")];
              additional_msg = [] }
            code
      | `MethodNotFound (e, m) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Method " ^ m.value ^ " not found in "
                ^ format_to_string e pp_expr;
              spans = [(m.span, "Method not found")];
              additional_msg = [] }
            code
      | `TypeError (expected, actual, span) ->
          let expected = format_to_string expected pp_type in
          let actual = format_to_string actual pp_type in
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Expected type `" ^ expected ^ "` but found `" ^ actual ^ "`";
              spans = [(span, "This has type `" ^ actual ^ "`")];
              additional_msg = [] }
            code
      | `ExpectedFunction (got, span) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Expected function but got value with `"
                ^ format_to_string got pp_type
                ^ "` type.";
              spans = [(span, "This cannot be called")];
              additional_msg = [] }
            code
      | `OnlyFunctionIsAllowed span ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Only function is allowed ";
              spans = [(span, "Only function is allowed here")];
              additional_msg = [] }
            code
      | `ArgumentNumberMismatch (expected, actual, span) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Expected " ^ string_of_int expected ^ " arguments but found "
                ^ string_of_int actual ^ ".";
              spans = [(span, "When calling this function")];
              additional_msg = [] }
            code
      | `UninterpretableStatement (_, span) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Uninterpretable statement.";
              spans = [(span, "This statement cannot be interpreted")];
              additional_msg = [] }
            code
      | `FieldNotFoundF field | `FieldNotFound (_, field) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Field `" ^ field.value ^ "` not found.";
              spans = [(field.span, "This field not found")];
              additional_msg = [] }
            code
      | `MissingField (_, field, span) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Field `" ^ field.value
                ^ "` missing in destructuring statement.";
              spans = [(span, "In this binding")];
              additional_msg = [] }
            code
      | `CannotHaveMethods (expr, ty) ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Type `"
                ^ format_to_string ty pp_type
                ^ "` cannot have methods.";
              spans = [(expr.span, "This cannot have methods")];
              additional_msg = [] }
            code
      | `IsNotStruct expr ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Expression is not struct type, so it cannot be used in such \
                 context.";
              spans = [(expr.span, "This is not struct type")];
              additional_msg = [] }
            code
      | `IsNotUnion expr ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg =
                "Expression is not union type, so it cannot be used in such \
                 context.";
              spans = [(expr.span, "This is not union type")];
              additional_msg = [] }
            code
      | `CaseNotFound span ->
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg = "Case type not found in union.";
              spans =
                [ ( span,
                    "Type of this variable is not found in the condition union"
                  ) ];
              additional_msg = [] }
            code
      | `ExpectedTypeFunction (is_type_fn, span) ->
          let diagnostic_msg =
            if is_type_fn then
              "Function should be called using `[]` brackets but called with \
               `()` parens."
            else
              "Function should be called using `()` brackets but called with \
               `[]` parens."
          in
          DiagnosticMsg.show f
            { severity = `Error;
              diagnostic_id = 1;
              diagnostic_msg;
              spans = [(span, "When calling this function")];
              additional_msg = [] }
            code
  end
