let parse_program s =
  Tact.Parser.program Tact.Lexer.token (Lexing.from_string s)

let test_empty () =
  let source = {||} in
  Alcotest.(check bool)
    "no bindings" true
    (match parse_program source with {bindings = []} -> true | _ -> false)

let test_let_type () =
  let source = {|
  let MyType = type {};
  |} in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    {value = Type {fields = []; type_bindings = []}; _} };
              _ } ] } ->
        true
    | _ ->
        false )

let test_let_type_param () =
  let source = {|
  let MyType(T: Type) = type {};
  |} in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "T"; _},
                                      {value = Reference (Ident "Type"); _} );
                                  _ } ];
                            returns = {value = Reference (Ident "Type"); _};
                            exprs =
                              Some
                                [ { value =
                                      Type {fields = []; type_bindings = []};
                                    _ } ];
                            _ };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type () =
  let source = {|
  type MyType {}
  |} in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    {value = Type {fields = []; type_bindings = []}; _} };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_constructor () =
  let source =
    {|
  type MyType { 
     a: Int257,
     b: Int257
  }
  let my = MyType {
    a: 0,
    b: 1
  };
  |}
  in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Type
                          { fields =
                              [ { value =
                                    { field_name = {value = Ident "a"; _};
                                      field_type =
                                        {value = Reference (Ident "Int257"); _}
                                    };
                                  _ };
                                { value =
                                    { field_name = {value = Ident "b"; _};
                                      field_type =
                                        {value = Reference (Ident "Int257"); _}
                                    };
                                  _ } ];
                            _ };
                      _ } };
              _ };
            { value =
                { binding_name = {value = Ident "my"; _};
                  binding_expr =
                    { value =
                        TypeConstructor
                          { constructor_id =
                              Some {value = Reference (Ident "MyType"); _};
                            fields_construction =
                              [ ({value = Ident "a"; _}, {value = Int _; _});
                                ({value = Ident "b"; _}, {value = Int _; _}) ]
                          };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_param () =
  let source = {|
  type MyType(T: Type) {}
  |} in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "T"; _},
                                      {value = Reference (Ident "Type"); _} );
                                  _ } ];
                            returns = {value = Reference (Ident "Type"); _};
                            exprs =
                              Some
                                [ { value =
                                      Type {fields = []; type_bindings = []};
                                    _ } ];
                            _ };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_fields_source =
  {|
  type MyType {
    a: Int257,
    f: get_type()
  }
  |}

let test_type_fields_trailing_comma_source =
  {|
  type MyType {
    a: Int257,
    f: get_type(),
  }
  |}

let test_type_fields source () =
  Alcotest.(check bool)
    "type fields" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Type
                          { fields =
                              [ { value =
                                    { field_name = {value = Ident "a"; _};
                                      field_type =
                                        {value = Reference (Ident "Int257"); _}
                                    };
                                  _ };
                                { value =
                                    { field_name = {value = Ident "f"; _};
                                      field_type =
                                        { value =
                                            FunctionCall
                                              { fn =
                                                  { value =
                                                      Reference
                                                        (Ident "get_type");
                                                    _ };
                                                arguments = [] };
                                          _ } };
                                  _ } ];
                            type_bindings = [] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_shorthand_fields () =
  let source = {|
    type MyType {
      A,
      B
    }
  |} in
  Alcotest.(check bool)
    "type fields" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Type
                          { fields =
                              [ { value =
                                    { field_name = {value = Ident "A"; _};
                                      field_type =
                                        {value = Reference (Ident "A"); _} };
                                  _ };
                                { value =
                                    { field_name = {value = Ident "B"; _};
                                      field_type =
                                        {value = Reference (Ident "B"); _} };
                                  _ } ];
                            type_bindings = [] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_methods () =
  let source =
    {|
    type MyType {
      fn test() -> Bool {}
      fn todo() -> Int257
    }
  |}
  in
  Alcotest.(check bool)
    "type methods" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Type
                          { fields = [];
                            type_bindings =
                              [ { value =
                                    { binding_name = {value = Ident "test"; _};
                                      binding_expr =
                                        { value =
                                            Function
                                              { name = None;
                                                params = [];
                                                exprs = Some [];
                                                returns =
                                                  { value =
                                                      Reference (Ident "Bool");
                                                    _ };
                                                _ };
                                          _ } };
                                  _ };
                                { value =
                                    { binding_name = {value = Ident "todo"; _};
                                      binding_expr =
                                        { value =
                                            Function
                                              { name = None;
                                                params = [];
                                                exprs = None;
                                                returns =
                                                  { value =
                                                      Reference (Ident "Int257");
                                                    _ };
                                                _ };
                                          _ } };
                                  _ } ] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_with_fields_and_methods_source =
  {|
    type MyType {
      a: Int257
      fn test() -> Bool {}
    }
  |}

let test_type_with_fields_and_methods_trailing_comma_source =
  {|
    type MyType {
      a: Int257,
      fn test() -> Bool {}
    }
  |}

let test_type_with_fields_and_methods source () =
  Alcotest.(check bool)
    "type methods" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "MyType"; _};
                  binding_expr =
                    { value =
                        Type
                          { fields =
                              [ { value =
                                    { field_name = {value = Ident "a"; _};
                                      field_type =
                                        {value = Reference (Ident "Int257"); _}
                                    };
                                  _ } ];
                            type_bindings =
                              [ { value =
                                    { binding_name = {value = Ident "test"; _};
                                      binding_expr =
                                        { value =
                                            Function
                                              { name = None;
                                                params = [];
                                                exprs = Some [];
                                                returns =
                                                  { value =
                                                      Reference (Ident "Bool");
                                                    _ };
                                                _ };
                                          _ } };
                                  _ } ] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_fn () =
  let source = {|
  let F = fn (A: T) -> P(1) {};
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "A"; _},
                                      {value = Reference (Ident "T"); _} );
                                  _ } ];
                            exprs = Some [];
                            returns =
                              { value =
                                  FunctionCall
                                    { fn = {value = Reference (Ident "P"); _};
                                      arguments = [{value = Int _; _}] };
                                _ } };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_fn_shorthand () =
  let source = {|
  fn F(A: T) -> P(1) {}
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "A"; _},
                                      {value = Reference (Ident "T"); _} );
                                  _ } ];
                            exprs = Some [];
                            returns =
                              { value =
                                  FunctionCall
                                    { fn = {value = Reference (Ident "P"); _};
                                      arguments = [{value = Int _; _}] };
                                _ } };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_fn_sig_over_call () =
  let source = {|
  let F = fn (A: T) -> P(1);
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "A"; _},
                                      {value = Reference (Ident "T"); _} );
                                  _ } ];
                            exprs = None;
                            returns =
                              { value =
                                  FunctionCall
                                    { fn = {value = Reference (Ident "P"); _};
                                      arguments = [{value = Int _; _}] };
                                _ } };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_fn_sig_returns_fn_sig () =
  let source = {|
  let F = fn (A: T) -> (fn () -> T);
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params =
                              [ { value =
                                    ( {value = Ident "A"; _},
                                      {value = Reference (Ident "T"); _} );
                                  _ } ];
                            exprs = None;
                            returns =
                              { value =
                                  Function
                                    { name = None;
                                      params = [];
                                      returns =
                                        {value = Reference (Ident "T"); _};
                                      exprs = None };
                                _ } };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_fn_call_over_sig () =
  let source = {|
  let F = (fn (A: T) -> P)(1);
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        FunctionCall
                          { fn =
                              { value =
                                  Function
                                    { name = None;
                                      params =
                                        [ { value =
                                              ( {value = Ident "A"; _},
                                                { value = Reference (Ident "T");
                                                  _ } );
                                            _ } ];
                                      exprs = None;
                                      returns =
                                        {value = Reference (Ident "P"); _} };
                                _ };
                            arguments = [{value = Int _; _}] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_function_call () =
  let source = {|
  let F = func(1);
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        FunctionCall
                          { fn = {value = Reference (Ident "func"); _};
                            arguments = [{value = Int _; _}];
                            _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_function_call_in_alist_of_expr () =
  let source = {|
  let F = fn() -> T { 
       func(1);
  };
  |} in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "F"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params = [];
                            exprs =
                              Some
                                [ { value =
                                      FunctionCall
                                        { fn =
                                            {value = Reference (Ident "func"); _};
                                          arguments = [{value = Int _; _}];
                                          _ };
                                    _ } ];
                            returns = {value = Reference (Ident "T"); _} };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_let_in_function_body () =
  let source =
    {|
  let f = fn() -> Int257 { 
       let a = 1;
       return a;
  };
  |}
  in
  Alcotest.(check bool)
    "function signature" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "f"; _};
                  binding_expr =
                    { value =
                        Function
                          { name = None;
                            params = [];
                            exprs =
                              Some
                                [ { value =
                                      Let
                                        { value =
                                            { binding_name =
                                                {value = Ident "a"; _};
                                              binding_expr = {value = Int _; _}
                                            };
                                          _ };
                                    _ };
                                  {value = Return (Reference (Ident "a")); _} ];
                            returns = {value = Reference (Ident "Int257"); _} };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_if_empty_body_no_else () =
  let source = {|
  fn test() -> A {
    if (1) {}
  }
  |} in
  Alcotest.(check bool)
    "if expr empty body no else" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "test"; _};
                  binding_expr =
                    { value =
                        Function
                          { exprs =
                              Some
                                [ { value =
                                      If
                                        { condition = {value = Int _; _};
                                          body = [];
                                          else_ = None };
                                    _ } ];
                            _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_if_has_body_with_else () =
  let source =
    {|
          fn test() -> A {
            if (1) {
              a;
            }
            else {}
          }
          |}
  in
  Alcotest.(check bool)
    "if expr has body with else" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "test"; _};
                  binding_expr =
                    { value =
                        Function
                          { exprs =
                              Some
                                [ { value =
                                      If
                                        { condition = {value = Int _; _};
                                          body =
                                            [{value = Reference (Ident "a"); _}];
                                          else_ =
                                            Some
                                              { value =
                                                  CodeBlock {block_exprs = []};
                                                _ } };
                                    _ } ];
                            _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_if_with_else_if () =
  let source =
    {|
    fn test() -> A {
      if (1) {}
      else if (10) {}
    }
    |}
  in
  Alcotest.(check bool)
    "if expr with else if" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "test"; _};
                  binding_expr =
                    { value =
                        Function
                          { exprs =
                              Some
                                [ { value =
                                      If
                                        { condition = {value = Int _; _};
                                          body = [];
                                          else_ = Some {value = If _; _} };
                                    _ } ];
                            _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_construction_function_call () =
  let source = {|
  let a = A(X) { field: value };
  |} in
  Alcotest.(check bool)
    "type construction function call" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_expr =
                    { value =
                        TypeConstructor
                          { constructor_id =
                              Some
                                { value =
                                    FunctionCall
                                      { fn = {value = Reference (Ident "A"); _};
                                        arguments =
                                          [{value = Reference (Ident "X"); _}];
                                        _ };
                                  _ };
                            fields_construction = _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let test_type_construction_type_declaration () =
  let source = {|
  let a = (type { field: Int257 }) { field: value };
  |} in
  Alcotest.(check bool)
    "type construction type declaration" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_expr =
                    { value =
                        TypeConstructor
                          { constructor_id =
                              Some
                                { value =
                                    Type
                                      { fields =
                                          [ { value =
                                                { field_name =
                                                    {value = Ident "field"; _};
                                                  field_type =
                                                    { value =
                                                        Reference
                                                          (Ident "Int257");
                                                      _ } };
                                              _ } ];
                                        _ };
                                  _ };
                            fields_construction = _ };
                      _ };
                  _ };
              _ } ] } ->
        true
    | _ ->
        false )

let () =
  let open Alcotest in
  run "Syntax"
    [ ("empty file", [test_case "Empty file" `Quick test_empty]);
      ( "type",
        [ test_case "let syntax for type" `Quick test_let_type;
          test_case "type constructor" `Quick test_type_constructor;
          test_case "let syntax for parameterized type" `Quick
            test_let_type_param;
          test_case "shorthand syntax for type" `Quick test_type;
          test_case "shorthand syntax for parameterized type" `Quick
            test_type_param;
          test_case "type fields" `Quick
            (test_type_fields test_type_fields_source);
          test_case "type fields with a trailing comma" `Quick
            (test_type_fields test_type_fields_trailing_comma_source);
          test_case "type shorthand fields" `Quick test_type_shorthand_fields;
          test_case "type methods" `Quick test_type_methods;
          test_case "type with fields and methods" `Quick
            (test_type_with_fields_and_methods
               test_type_with_fields_and_methods_source );
          test_case "type with fields and methods with a trailing comma" `Quick
            (test_type_with_fields_and_methods
               test_type_with_fields_and_methods_trailing_comma_source ) ] );
      ( "functions",
        [ test_case "function definition" `Quick test_fn;
          test_case "shorthand function definition" `Quick test_fn_shorthand;
          test_case "function signature over function call" `Quick
            test_fn_sig_over_call;
          test_case "function signature returning function signature" `Quick
            test_fn_sig_returns_fn_sig;
          test_case "function call over function signature" `Quick
            test_fn_call_over_sig;
          test_case "let in function body" `Quick test_let_in_function_body ] );
      ( "function calls",
        [ test_case "function call" `Quick test_function_call;
          test_case "function call in a list of exprs" `Quick
            test_function_call_in_alist_of_expr ] );
      ( "if stmts",
        [ test_case "if expr empty body no else" `Quick
            test_if_empty_body_no_else;
          test_case "if expr has body with else" `Quick
            test_if_has_body_with_else;
          test_case "if expr with else if" `Quick test_if_with_else_if ] );
      ( "type construction",
        [ test_case "type construction function call" `Quick
            test_type_construction_function_call;
          test_case "type construction type declaration" `Quick
            test_type_construction_type_declaration ] ) ]
