module Make (Config : Config.T) = struct
  open MParser
  open Base
  module Syntax = Syntax.Make (Config)
  open Syntax

  (* TODO: review for correctness *)
  let locate value =
    pipe3 get_pos value get_pos
      (fun (_index0, _line0, _column0) value (_index, _line, _column) ->
        Syntax.make_located ~value
          ~span:
            ( { pos_fname = "";
                pos_lnum = _line0;
                pos_bol = _column0;
                pos_cnum = _column0 },
              { pos_fname = "";
                pos_lnum = _line;
                pos_bol = _column;
                pos_cnum = _column } )
          () )

  let many_chars_starting_with lead p =
    pipe2 lead (many p) (fun lead rest -> lead :: rest) |>> String.of_char_list

  let any_char_but_nl_or_eof s =
    match read_char s with
    | Some '\n' | Some '\r' | None ->
        Empty_failed (expected_error s "any char but newline or eof")
    | Some c ->
        Consumed_ok (c, advance_state s 1, No_error)

  let comment_line = skip_string "//" >> skip_many any_char_but_nl_or_eof

  let rec comment_block state =
    ( skip_string "/*"
    >> skip_many_until
         (comment_block <|> skip any_char_or_nl)
         (skip_string "*/") )
      state

  let whitespace = skip_many (comment_line <|> comment_block <|> spaces1)

  let between left right p = left >>> p <<< right

  let ( !! ) p = whitespace >> p << whitespace

  let parens p = between (char '(') (char ')') p

  let comma s = char ',' s

  let comma_sep p = sep_by p comma

  let ( >>> ) x y = whitespace >> x >> whitespace >> y << whitespace

  let ( <<< ) x y = whitespace >> x << whitespace << y << whitespace

  let infix sym f assoc = Infix (sym |>> f, assoc)

  let prefix sym f = Prefix (skip_string sym >>> return f)

  let rec keyword state =
    ( string "as" <|> string "let" <|> string "interface" <|> string "impl"
    <|> string "struct" <|> string "enum" <|> string "union" <|> string "fn"
    <|> string "fn" <|> string "if" <|> string "else" <|> string "return"
    <|> string "val" <|> string "case" <|> string "switch"
    >> not_followed_by ident' "identifier" )
      state

  and skip_keyword keyword =
    skip_string keyword >> not_followed_by ident' "identifier"

  and integer state =
    ( many_chars_starting_with digit (digit <|> char '_')
    |>> fun x -> Int (Zint.of_string x) )
      state

  and ident' state =
    (* alphanumeric, underscore starting with letters and underscore only *)
    ( many_chars_starting_with (letter <|> char '_') (alphanum <|> char '_')
    |>> fun s -> Ident s )
      state

  and ident state =
    (* should not be a reserved keyword *)
    (not_followed_by keyword "identifier (uses a reserved keyword)" >> ident')
      state

  and attribute state =
    (pipe2
       (attempt (skip_char '@' >> locate ident))
       (option (parens (comma_sep !!(locate expr))))
       (fun attribute_ident -> function
         | None ->
             make_attribute ~attribute_ident ()
         | Some attribute_exprs ->
             make_attribute ~attribute_ident ~attribute_exprs () ) )
      state

  and attributes state = (many attribute) state

  and struct_field state =
    ( locate
        !!(pipe3 attributes
             (skip_keyword "val" >>> locate ident <<< char ':')
             (locate opless_expr)
             (fun field_attributes field_name field_type ->
               make_struct_field ~field_attributes ~field_name ~field_type () ) )
    |>> fun x -> `Field x )
      state

  and struct_item state =
    (struct_field <<< (skip_char ';' <|> look_ahead (skip_char '}'))) state

  and gen_struct :
        'a. ('a, 's) t -> 's state -> ('a * struct_definition, 's) reply =
   fun name state ->
    ( !!(locate
           (pair attributes
              (pair
                 (skip_keyword "struct" >>> name)
                 (char '{' >>> many !!struct_item <<< char '}') ) ) )
    |>> fun v ->
    let struct_attributes, (name, items) = Syntax.value v in
    ( name,
      make_struct_definition ~struct_attributes
        ~fields:
          (List.filter_map items ~f:(function `Field f -> Some f | _ -> None))
        ~struct_bindings:[] ~impls:[] ~struct_span:(Syntax.span v) () ) )
      state

  and struct_ state = (gen_struct (return ()) |>> snd) state

  and struct_stmt state =
    ( gen_struct (locate ident)
    |>> fun (binding_name, struct_) ->
    let span = Syntax.span_to_concrete struct_.struct_span in
    Let
      (Syntax.make_located ~span
         ~value:
           (make_binding ~binding_name
              ~binding_expr:
                (Syntax.make_located ~span ~value:(Struct struct_) ())
              () )
         () ) )
      state

  and operators =
    let op name _operator l r =
      Syntax.map_located _operator ~f:(fun _ ->
          MethodCall
            (make_method_call ~receiver:l
               ~receiver_fn:(Syntax.builtin_located (Ident name))
               ~receiver_arguments:[r] () ) )
    in
    [ [ infix (locate (string "*")) (op "mul") Assoc_left;
        infix (locate (string "/")) (op "div") Assoc_left ];
      [ infix (locate (string "+")) (op "add") Assoc_left;
        infix (locate (string "-")) (op "subtract") Assoc_left ] ]

  and opless_expr state =
    ( integer
    <|> (struct_ |>> fun x -> Struct x)
    <|> (locate ident |>> fun x -> Reference x)
    <|> parens expr )
      state

  and expr state =
    (expression operators (locate !!opless_expr) |>> Syntax.value) state

  and let_ state =
    ( locate
        ( skip_keyword "let"
        >>> pipe2
              (locate ident <<< char '=')
              (locate expr <<< char ';')
              (fun binding_name binding_expr -> {binding_name; binding_expr}) )
    |>> fun x -> Let x )
      state

  and stmt state =
    ( (* any statement can have leading whitespace *) whitespace
    >> ( (* `let` statement *)
         let_ <|> (* `struct` statement *) attempt struct_stmt
       <|> (* any expression is also a statement *)
       (locate expr <<< (skip_char ';' <|> eof) |>> fun e -> Expr e) ) )
      state

  and program state = (many !!stmt << eof) state

  let parse (s : string) : stmt list =
    match parse_string program s () with
    | Success e ->
        e
    | Failed (msg, _) ->
        Caml.print_string msg ; failwith msg
end
