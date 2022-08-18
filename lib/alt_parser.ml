module Make (Config : Config.T) = struct
  open MParser
  open Base
  module Syntax = Syntax.Make (Config)
  open Syntax

  let locate value =
    pipe3 get_pos value get_pos
      (fun (_index0, _line0, _column0) value (_index, _line, _column) ->
        Syntax.make_located ~value
          ~span:
            ( { pos_fname = "";
                pos_lnum = _line0;
                pos_bol = _column0;
                pos_cnum = _index0 },
              { pos_fname = "";
                pos_lnum = _line;
                pos_bol = _column;
                pos_cnum = _index } )
          () )

  let chain p op = p >>= fun x -> many_fold_left (fun x f -> f x) x op

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
         (skip_string "*/")
    <?> "block comment" )
      state

  let whitespace = skip_many (comment_line <|> comment_block <|> spaces1)

  let between left right p = left >>> p <<< right

  let ( !! ) p = whitespace >> p << whitespace

  let parens p = between (char '(') (char ')') p

  let brackets p = between (char '[') (char ']') p

  let comma s = char ',' s

  let comma_sep p = sep_by p comma

  let present p = option p |>> Option.is_some

  let ( >>> ) x y = whitespace >> x >> whitespace >> y << whitespace

  let ( <<< ) x y = whitespace >> x << whitespace << y << whitespace

  let infix sym f assoc = Infix (sym |>> f, assoc)

  let postfix sym f = Postfix (sym |>> f)

  let prefix sym f = Prefix (sym |>> f)

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

  and bool_ state =
    ( skip_keyword "true"
    |>> (fun _ -> true)
    <|> (skip_keyword "false" |>> fun _ -> false) )
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

  and parameters state =
    (brackets
       (many (locate (pair !!(locate ident <<< char ':') !!(locate expr)))) )
      state

  and parameterization state =
    let parameterize arguments expr =
      Syntax.map_located expr ~f:(fun _ ->
          Function
            (make_function_definition (* FIXME: not sure it's a good span *)
               ~function_def_span:expr.span ~params:arguments
               ~is_type_function:true
               ~function_body:
                 (make_function_body
                    ~function_stmt:{value = Expr expr; span = expr.span}
                    () )
               () ) )
    in
    (chain
       !!(attempt parameters |>> parameterize <|> return (fun x -> x))
       !!( parameters
         |>> fun arguments mk expr -> mk (parameterize arguments expr) ) )
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
             (locate expr)
             (fun field_attributes field_name field_type ->
               make_struct_field ~field_attributes ~field_name ~field_type () ) )
    |>> fun x -> `Field x )
      state

  and struct_item state =
    ( struct_field
    <|> (fn_stmt |>> fun x -> `Fn x)
    <<< (attempt (skip_char ';') <|> whitespace <|> look_ahead (skip_char '}'))
    )
      state

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
        ~struct_bindings:
          (List.filter_map items ~f:(function
            | `Fn (Let f) ->
                Some f
            | _ ->
                None ) )
        ~impls:[] ~struct_span:(Syntax.span v) () ) )
      state

  and struct_ state =
    ( gen_struct parameterization
    |>> fun (parameterize, struct_) ->
    parameterize
      (Syntax.make_located
         ~span:(Syntax.span_to_concrete struct_.struct_span)
         ~value:(Struct struct_) () ) )
      state

  and struct_stmt state =
    ( gen_struct (pair !!(locate ident) parameterization)
    |>> fun ((binding_name, parameterize), struct_) ->
    let span = Syntax.span_to_concrete struct_.struct_span in
    Let
      (Syntax.make_located ~span
         ~value:
           (make_binding ~binding_name
              ~binding_expr:
                (parameterize
                   (Syntax.make_located ~span ~value:(Struct struct_) ()) )
              () )
         () ) )
      state

  and interface_item state =
    ( fn_stmt
    |>> (fun x -> `Fn x)
    <<< (attempt (skip_char ';') <|> whitespace <|> look_ahead (skip_char '}'))
    )
      state

  and gen_interface :
        'a. ('a, 's) t -> 's state -> ('a * interface_definition, 's) reply =
   fun name state ->
    ( !!(locate
           (pair attributes
              (pair
                 (skip_keyword "interface" >>> name)
                 (char '{' >>> many !!interface_item <<< char '}') ) ) )
    |>> fun v ->
    let interface_attributes, (name, items) = Syntax.value v in
    ( name,
      make_interface_definition ~interface_attributes
        ~interface_members:
          (List.filter_map items ~f:(function
            | `Fn (Let f) ->
                Some f
            | _ ->
                None ) )
        () ) )
      state

  and interface state = (gen_interface (return ()) |>> snd) state

  and interface_stmt state =
    ( locate (gen_interface (pair !!(locate ident) parameterization))
    |>> fun v ->
    let span = Syntax.span_to_concrete @@ Syntax.span v in
    let (binding_name, parameterize), interface_ = Syntax.value v in
    Let
      (Syntax.make_located ~span
         ~value:
           (make_binding ~binding_name
              ~binding_expr:
                (parameterize
                   (Syntax.make_located ~span ~value:(Interface interface_) ()) )
              () )
         () ) )
      state

  and named_param state =
    (pair !!(locate ident <<< char ':') !!(locate expr)) state

  and function_parameters state =
    (parens (comma_sep (locate named_param))) state

  and gen_fn :
        'a. ('a, 's) t -> 's state -> ('a * function_definition, 's) reply =
   fun name state ->
    !!( locate
          (pair attributes
             (pair
                (pair (skip_keyword "fn" >>> name) function_parameters)
                (pair
                   ( attempt
                       (option
                          ( skip_string "->"
                          >>> locate (expr ~struct_construction_allowed:false)
                          ) )
                   <|> return None )
                   (option (locate code_block)) ) ) )
      |>> fun v ->
      let function_attributes, ((name, params), (returns, function_body)) =
        Syntax.value v
      in
      ( name,
        make_function_definition ~function_attributes ~is_type_function:false
          ~params ?returns
          ?function_body:
            (Option.map function_body ~f:(fun function_body ->
                 {function_stmt = function_body} ) )
          ~function_def_span:(Syntax.span v) () ) )
      state

  and fn state = (gen_fn (return ()) |>> snd) state

  and fn_stmt state =
    ( gen_fn (pair !!(locate ident) parameterization)
    |>> fun ((binding_name, parameterize), fn) ->
    let span = Syntax.span_to_concrete fn.function_def_span in
    Let
      (Syntax.make_located ~span
         ~value:
           (make_binding ~binding_name
              ~binding_expr:
                (parameterize
                   (Syntax.make_located ~span ~value:(Function fn) ()) )
              () )
         () ) )
      state

  and type_index state = !!(brackets (comma_sep (locate expr))) state

  and function_index state = !!(parens (comma_sep (locate expr))) state

  and field_access state = (char '.' >>> locate ident) state

  and struct_construction state =
    (char '{' >>> comma_sep named_param <<< char '}') state

  and expr ?(struct_construction_allowed = true) state =
    let opless_expr =
      integer
      <|> (attempt bool_ |>> fun x -> Bool x)
      <|> (struct_ |>> Syntax.value)
      <|> (locate ident |>> fun x -> Reference x)
      <|> (fn |>> fun x -> Function x)
      <|> (interface |>> fun x -> Interface x)
      <|> parens expr
    and operators =
      let op name _operator l r =
        Syntax.map_located _operator ~f:(fun _ ->
            MethodCall
              (make_method_call ~receiver:l
                 ~receiver_fn:(Syntax.builtin_located (Ident name))
                 ~receiver_arguments:[r] () ) )
      and negate _operator e =
        Syntax.map_located _operator ~f:(fun _ ->
            match Syntax.value e with
            | Int x ->
                Int (Z.neg x)
            | _ ->
                MethodCall
                  (make_method_call ~receiver:e
                     ~receiver_fn:(Syntax.builtin_located (Ident "neg"))
                     ~receiver_arguments:[] () ) )
      in
      [ [prefix (locate (string "-")) negate];
        [ infix (locate (string "*")) (op "mul") Assoc_left;
          infix (locate (string "/")) (op "div") Assoc_left ];
        [ infix (locate (string "+")) (op "add") Assoc_left;
          infix (locate (string "-")) (op "subtract") Assoc_left ] ]
    in
    (* handle type and function indices, method calls and field access  *)
    let exp =
      let funcall is_type_func_call arguments l =
        Syntax.map_located l ~f:(fun l' ->
            match l' with
            | FieldAccess {from_expr; to_field; _} ->
                MethodCall
                  (make_method_call ~receiver:from_expr ~receiver_fn:to_field
                     ~receiver_arguments:arguments () )
            | _ ->
                FunctionCall
                  (make_function_call ~fn:l ~arguments ~is_type_func_call ()) )
      and make_field_access to_field from_expr =
        Syntax.map_located to_field ~f:(fun _ ->
            FieldAccess (make_field_access ~from_expr ~to_field ()) )
      in
      let rhs' =
        type_index |>> funcall true
        <|> (function_index |>> funcall false)
        <|> (field_access |>> make_field_access)
      in
      let rhs =
        if struct_construction_allowed then
          rhs'
          <|> ( struct_construction
              |>> fun fields_construction constructor_id ->
              (* FIXME: this span is wrong *)
              Syntax.map_located constructor_id ~f:(fun _ ->
                  StructConstructor {fields_construction; constructor_id} ) )
        else rhs'
      in
      chain (expression operators (locate !!opless_expr)) (attempt rhs)
    in
    (* handle operators *)
    (expression operators exp |>> Syntax.value) state

  and let_ state =
    ( locate
        ( skip_keyword "let"
        >>> pipe2
              (pair (locate ident) parameterization <<< char '=')
              (locate expr <<< char ';')
              (fun (binding_name, parameterize) binding_expr ->
                {binding_name; binding_expr = parameterize binding_expr} ) )
    |>> fun x -> Let x )
      state

  and destructing_let_stmt state =
    let destructuring_binding state =
      let bind state =
        (pipe2 (locate ident)
           (option (skip_keyword "as" >>> locate ident))
           (fun id1 id2 ->
             match id2 with Some id2 -> (id1, id2) | None -> (id1, id1) ) )
          state
      in
      ( char '{'
      >>> locate
            (comma_sep !!(attempt (bind |>> fun b -> Some b) <|> return None))
      >>= (fun bindings ->
            ( match Syntax.value bindings |> List.rev with
            | [] | [None] ->
                present !!(skip_string "..")
            | _ :: l ->
                (* TODO: improve this error state handling, it is a bit of a mess *)
                if List.exists l ~f:Option.is_none then fun s ->
                  Empty_failed
                    (unexpected_error s "leading comma prior to this input")
                else present !!(skip_string "..") )
            |>> fun rest ->
            ( Syntax.map_located bindings ~f:(fun bindings ->
                  List.filter_map bindings ~f:(fun x -> x) ),
              rest ) )
      <<< char '}' )
        state
    in
    ( locate
        ( skip_keyword "let"
        >>> pipe2
              (destructuring_binding <<< char '=')
              (locate expr)
              (fun (destructuring_binding, destructuring_binding_rest)
                   destructuring_binding_expr ->
                { destructuring_binding;
                  destructuring_binding_rest;
                  destructuring_binding_expr } ) )
    |>> fun x -> DestructuringLet x )
      state

  (* Sequence of statements that may end with a non-semicolon terminated result *)
  and stmt_seq ?(f = fun s -> Break s) state =
    (pipe2
       !!(many (attempt (locate stmt)))
       !!(option
            ( locate expr
            |>> fun e ->
            Syntax.map_located e ~f:(fun _ ->
                f (Syntax.map_located e ~f:(fun _ -> Expr e)) ) ) )
       (fun stmts -> function None -> stmts | Some return -> stmts @ [return]) )
      state

  and block_stmt state = (char '{' >>> stmt_seq <<< char '}') state

  and code_block state = (block_stmt |>> fun stmts -> CodeBlock stmts) state

  (* SWITCH stmt *)
  and switch_branch state =
    ( skip_keyword "case"
    >>> pipe3 (locate expr)
          (locate ident <<< skip_string "=>")
          (locate code_block)
          (fun ty var stmt -> {ty; var; stmt}) )
      state

  and default_branch state =
    (skip_keyword "else" >>> skip_string "=>" >>> code_block) state

  and switch state =
    ( skip_keyword "switch"
    >>> pipe2
          (char '(' >>> locate expr <<< char ')')
          ( char '{'
          >>> pipe2
                (many (locate switch_branch))
                (option default_branch)
                (fun x y -> (x, y))
          <<< char '}' )
          (fun switch_condition (branches, default) ->
            Switch {switch_condition; branches; default} ) )
      state

  (* WHILE expr *)
  and while_loop state =
    ( skip_keyword "while"
    >>> pipe2
          (char '(' >>> locate expr <<< char ')')
          (locate code_block)
          (fun while_cond while_body -> WhileLoop {while_cond; while_body}) )
      state

  (* IF-ELSE expr *)
  and if_expr state =
    ( skip_keyword "if"
    >>> pipe3
          (char '(' >>> locate expr <<< char ')')
          (locate code_block)
          (option (skip_keyword "else" >>> locate code_block))
          (fun condition body else_ -> {condition; body; else_}) )
      state

  and if_stmt state = (if_expr |>> fun x -> If x) state

  (* RETURN expr *)
  and return_expr state =
    (skip_keyword "return" >>> locate expr |>> fun x -> Return x) state

  (* ASSIGNMENT expr *)
  and assignment_stmt state =
    (pipe3 (locate ident)
       !!(char '=')
       (locate expr)
       (fun assignment_ident _ assignment_expr ->
         Assignment {assignment_ident; assignment_expr} ) )
      state

  and stmt_expr state = (locate expr |>> fun e -> Expr e) state

  and semicolon_stmt state =
    ( destructing_let_stmt <|> return_expr <|> attempt assignment_stmt
    <|> stmt_expr <<< char ';' )
      state

  and stmt state =
    ( whitespace
    >> ( attempt let_ <|> attempt struct_stmt <|> attempt interface_stmt
       <|> attempt fn_stmt <|> switch <|> while_loop <|> semicolon_stmt
       <|> if_stmt <|> code_block ) )
      state

  and program state = (stmt_seq ~f:Syntax.value << eof) state

  let parse (s : string) : program =
    match parse_string program s () with
    | Success e ->
        {stmts = e}
    | Failed (msg, _) ->
        Caml.print_string msg ; failwith msg
end
