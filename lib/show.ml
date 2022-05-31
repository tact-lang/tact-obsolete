module Make =
functor
  (Syntax : Syntax.T)
  ->
  struct
    open Caml.Format
    module Lang = Lang.Make (Syntax)

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
     fun f -> function
      | Value v ->
          pp_value f v
      | FunctionCall (fname, args) ->
          pp_expr f fname ;
          pp_print_string f "(" ;
          list_iter args
            ~f:(fun e -> pp_expr f e ; pp_print_string f ", ")
            ~flast:(fun e -> pp_expr f e) ;
          pp_print_string f ")"
      | Reference (name, _) | ResolvedReference (name, _) ->
          pp_print_string f name
      | StructField (s, field) ->
          pp_expr f s ; pp_print_string f "." ; pp_print_string f field
      | _ ->
          pp_print_string f "<anonymous>"

    and pp_value f = function
      | Integer i ->
          Z.pp_print f i
      | Builtin b ->
          pp_print_string f b
      | Type t ->
          pp_type f t
      | _ ->
          pp_print_string f "<anonymous>"

    and pp_type f = function
      | TypeType ->
          pp_print_string f "Type"
      | IntegerType ->
          pp_print_string f "Integer"
      | BoolType ->
          pp_print_string f "Bool"
      | VoidType ->
          pp_print_string f "VoidType"
      | BuiltinType t ->
          pp_print_string f t
      | StructType _ ->
          pp_print_string f "<struct>"
      | _ ->
          pp_print_string f "<anonymous>"

    let show_error : error -> string = function
      | `DuplicateField (field, s) ->
          Printf.sprintf "Duplicate field `%s` in the `%i` struct." field
            s.struct_id
      | `UnresolvedIdentifier id ->
          Printf.sprintf "Unresolved identifier `%s`." id
      | `MethodNotFound (e, m) ->
          Printf.sprintf "Method `%s` not found in `%s` expr." m
            (format_to_string e pp_expr)
      | `UnexpectedType t ->
          Printf.sprintf "Unexpected type `%s`." (format_to_string t pp_expr)
      | `TypeError (expected, actual) ->
          Printf.sprintf "Type error: expected `%s` but found `%s`."
            (format_to_string expected pp_expr)
            (format_to_string actual pp_expr)
      | `ExpectedFunction got ->
          Printf.sprintf "Expected function but got `%s`."
            (format_to_string got pp_expr)
      | `UnallowedStmt _ ->
          Printf.sprintf "Unallowed statement at top-level."
      | `OnlyFunctionIsAllowed ->
          Printf.sprintf "Expected function."
      | `ArgumentNumberMismatch ->
          Printf.sprintf "Argument number mismatch."
      | `UninterpretableStatement _ ->
          Printf.sprintf "Uninterpretable statement."
      | `FieldNotFound (_, field) ->
          Printf.sprintf "Field `%s` not found." field
  end