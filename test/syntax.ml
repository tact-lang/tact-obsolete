let parse_program s =
  Tact.Parser.program Tact.Lexer.token (Lexing.from_string s)

let test_empty () =
  let source = {||} in
  Alcotest.(check bool)
    "no bindings" true
    (match parse_program source with {bindings = []} -> true | _ -> false)

let test_let_struct () =
  let source = {|
  let Struct = struct {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
                  binding_expr =
                    {value = Struct {fields = []; struct_bindings = []}; _} };
              _ } ] } ->
        true
    | _ ->
        false )

let test_let_struct_param () =
  let source = {|
  let Struct(T: Type) = struct {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
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
                              [ { value =
                                    Struct {fields = []; struct_bindings = []};
                                  _ } ];
                            _ };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_struct () =
  let source = {|
  struct Struct {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
                  binding_expr =
                    {value = Struct {fields = []; struct_bindings = []}; _} };
              _ } ] } ->
        true
    | _ ->
        false )

let test_struct_param () =
  let source = {|
  struct Struct(T: Type) {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
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
                              [ { value =
                                    Struct {fields = []; struct_bindings = []};
                                  _ } ];
                            _ };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_struct_fields_source =
  {|
  struct Struct {
    a: Int257,
    f: get_type()
  }
  |}

let test_struct_fields_trailing_comma_source =
  {|
  struct Struct {
    a: Int257,
    f: get_type(),
  }
  |}

let test_struct_fields source () =
  Alcotest.(check bool)
    "struct fields" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
                  binding_expr =
                    { value =
                        Struct
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
                            struct_bindings = [] };
                      _ } };
              _ } ] } ->
        true
    | _ ->
        false )

let test_struct_methods () =
  let source = {|
    struct Struct {
      fn test(): Bool {}
    }
  |} in
  Alcotest.(check bool)
    "struct methods" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
                  binding_expr =
                    { value =
                        Struct
                          { fields = [];
                            struct_bindings =
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

let test_struct_with_fields_and_methods_source =
  {|
    struct Struct {
      a: Int257
      fn test(): Bool {}
    }
  |}

let test_struct_with_fields_and_methods_trailing_comma_source =
  {|
    struct Struct {
      a: Int257,
      fn test(): Bool {}
    }
  |}

let test_struct_with_fields_and_methods source () =
  Alcotest.(check bool)
    "struct methods" true
    ( match parse_program source with
    | { bindings =
          [ { value =
                { binding_name = {value = Ident "Struct"; _};
                  binding_expr =
                    { value =
                        Struct
                          { fields =
                              [ { value =
                                    { field_name = {value = Ident "a"; _};
                                      field_type =
                                        {value = Reference (Ident "Int257"); _}
                                    };
                                  _ } ];
                            struct_bindings =
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
      ( "struct",
        [ test_case "let syntax for struct" `Quick test_let_struct;
          test_case "let syntax for parameterized struct" `Quick
            test_let_struct_param;
          test_case "shorthand syntax for struct" `Quick test_struct;
          test_case "shorthand syntax for parameterized struct" `Quick
            test_struct_param;
          test_case "struct fields" `Quick
            (test_struct_fields test_struct_fields_source);
          test_case "struct fields with a trailing comma" `Quick
            (test_struct_fields test_struct_fields_trailing_comma_source);
          test_case "struct methods" `Quick test_struct_methods;
          test_case "struct with fields and methods" `Quick
            (test_struct_with_fields_and_methods
               test_struct_with_fields_and_methods_source );
          test_case "struct with fields and methods with a trailing comma"
            `Quick
            (test_struct_with_fields_and_methods
               test_struct_with_fields_and_methods_trailing_comma_source ) ] )
    ]
