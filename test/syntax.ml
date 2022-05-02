let parse_program s =
  Tact.Parser.program Tact.Lexer.token (Lexing.from_string s)

let test_empty () =
  let source = {||} in
  Alcotest.(check bool)
    "no bindings" true
    (match parse_program source with {bindings = []} -> true | _ -> false)

let test_let_type () =
  let source = {|
  let MyType = type {}
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
  let MyType(T: Type) = type {}
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
                                    FunctionParam
                                      ( {value = Ident "T"; _},
                                        {value = Reference (Ident "Type"); _} );
                                  _ } ];
                            returns = {value = Reference (Ident "Type"); _};
                            exprs =
                              [ { value = Type {fields = []; type_bindings = []};
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
                                    FunctionParam
                                      ( {value = Ident "T"; _},
                                        {value = Reference (Ident "Type"); _} );
                                  _ } ];
                            returns = {value = Reference (Ident "Type"); _};
                            exprs =
                              [ { value = Type {fields = []; type_bindings = []};
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

let test_type_methods () =
  let source = {|
    type MyType {
      fn test(): Bool {}
    }
  |} in
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
                                                exprs = [];
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

let test_type_with_fields_and_methods_source =
  {|
    type MyType {
      a: Int257
      fn test(): Bool {}
    }
  |}

let test_type_with_fields_and_methods_trailing_comma_source =
  {|
    type MyType {
      a: Int257,
      fn test(): Bool {}
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
                                                exprs = [];
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

let () =
  let open Alcotest in
  run "Syntax"
    [ ("empty file", [test_case "Empty file" `Quick test_empty]);
      ( "type",
        [ test_case "let syntax for type" `Quick test_let_type;
          test_case "let syntax for parameterized type" `Quick
            test_let_type_param;
          test_case "shorthand syntax for type" `Quick test_type;
          test_case "shorthand syntax for parameterized type" `Quick
            test_type_param;
          test_case "type fields" `Quick
            (test_type_fields test_type_fields_source);
          test_case "type fields with a trailing comma" `Quick
            (test_type_fields test_type_fields_trailing_comma_source);
          test_case "type methods" `Quick test_type_methods;
          test_case "type with fields and methods" `Quick
            (test_type_with_fields_and_methods
               test_type_with_fields_and_methods_source );
          test_case "type with fields and methods with a trailing comma" `Quick
            (test_type_with_fields_and_methods
               test_type_with_fields_and_methods_trailing_comma_source ) ] ) ]
