open Base
open Tact.Lang

let parse_program s =
  Tact.Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program stx =
  let elist = make_error_list ~warnings:(ref []) ~errors:(ref []) () in
  let env = Tact.Lang.env_from_program stx elist in
  match (!(elist.errors), !(elist.warnings)) with
  | error :: _, _ ->
      Error error
  | _ ->
      Ok env

let test_scope_resolution () =
  let source =
    {|
  let I = Int257;
  let I_ = I;
  let n = 1;
  let n_ = n;
  |}
  in
  Alcotest.(check bool)
    "reference resolution" true
    ( match parse_program source |> build_program with
    | Ok {scope; _} ->
        [%matches? Some (ResolvedReference ("Int257", Builtin "Int257"))]
          (in_amap scope (fun m -> Map.find m "I"))
        && [%matches? Some (ResolvedReference ("I", Builtin "Int257"))]
             (in_amap scope (fun m -> Map.find m "I_"))
        && [%matches? Some (Integer _)] (in_amap scope (fun m -> Map.find m "n"))
        && [%matches? Some (ResolvedReference ("n", Integer _))]
             (in_amap scope (fun m -> Map.find m "n_"))
    | _ ->
        false )

let test_scope_resolution_stripped () =
  let source =
    {|
  let I = Int257;
  let I_ = I;
  let n = 1;
  let n_ = n;
  |}
  in
  Alcotest.(check bool)
    "reference resolution" true
    ( match parse_program source |> build_program with
    | Ok env ->
        let scope =
          ((new resolved_references_stripper env)#visit_env () env).scope
        in
        [%matches? Some (Builtin "Int257")]
          (in_amap scope (fun m -> Map.find m "I"))
        && [%matches? Some (Builtin "Int257")]
             (in_amap scope (fun m -> Map.find m "I_"))
        && [%matches? Some (Integer _)] (in_amap scope (fun m -> Map.find m "n"))
        && [%matches? Some (Integer _)]
             (in_amap scope (fun m -> Map.find m "n_"))
    | _ ->
        false )

let test_recursive_scope_resolution () =
  let source = {|
  let A = B;
  let B = C;
  let C = A;
  |} in
  Alcotest.(check bool)
    "reference resolution" true
    ( match parse_program source |> build_program with
    | Error (Recursive_Reference "C") ->
        true
    | _ ->
        false )

let test_type () =
  let source =
    {|
  let MyType = type {
       a: Int257,
       b: Bool
  };
  |}
  in
  Alcotest.(check bool)
    "type binding" true
    ( match parse_program source |> build_program with
    | Ok {scope; _} -> (
      match in_amap scope (fun m -> Map.find m "MyType") with
      | Some (Type s) ->
          [%matches?
            Some
              { field_type =
                  ResolvedReferenceKind ("Int257", BuiltinKind "Int257");
                _ }]
            (in_amap s.type_fields (fun m -> Map.find m "a"))
          && [%matches?
               Some
                 { field_type =
                     ResolvedReferenceKind ("Bool", BuiltinKind "Bool");
                   _ }]
               (in_amap s.type_fields (fun m -> Map.find m "b"))
      | _ ->
          false )
    | _ ->
        false )

let test_type_duplicate () =
  let source = {|
  let MyType = type {};
  let MyType = type {};
  |} in
  Alcotest.(check bool)
    "type binding" true
    ([%matches? Error (Duplicate_Identifier ("MyType", Type _))]
       (parse_program source |> build_program) )

let test_type_duplicate_non_type () =
  let source = {|
  let MyType = 1;
  let MyType = type {};
  |} in
  Alcotest.(check bool)
    "type binding" true
    ([%matches? Error (Duplicate_Identifier ("MyType", Integer _))]
       (parse_program source |> build_program) )

let test_type_duplicate_field () =
  let source =
    {|
  let MyType = type {
      a: Int257,
      a: Bool
  };
  |}
  in
  Alcotest.(check bool)
    "type binding" true
    ([%matches? Error (Duplicate_Field ("a", _))]
       (parse_program source |> build_program) )

let test_function () =
  let source =
    {|
      fn test(a: Int257, b: Bool) -> Int257 {
       1;
       2;
      }
  |}
  in
  Alcotest.(check bool)
    "function" true
    ( match parse_program source |> build_program with
    | Ok {scope; _} -> (
      match in_amap scope (fun m -> Map.find m "test") with
      | Some (Function f) ->
          [%matches?
            {function_params; _} when [%matches?
                                        [ ( "a",
                                            ResolvedReferenceKind
                                              ("Int257", BuiltinKind "Int257")
                                          );
                                          ( "b",
                                            ResolvedReferenceKind
                                              ("Bool", BuiltinKind "Bool") ) ]]
                                        function_params]
            f
          && [%matches?
               { function_returns =
                   ResolvedReferenceKind ("Int257", BuiltinKind "Int257");
                 _ }]
               f
          && [%matches?
               {function_body = Some [Value (Integer i); Value (Integer i')]; _} when 
               equal (Z.to_int i) 1 && equal (Z.to_int i') 2]
               f
      | _ ->
          false )
    | _ ->
        false )

let () =
  let open Alcotest in
  run "Lang"
    [ ( "identifiers",
        [ test_case "name resolution in the scope" `Quick test_scope_resolution;
          test_case "stripped name resolution in the scope" `Quick
            test_scope_resolution_stripped;
          test_case "recursive name resolution in the scope" `Quick
            test_recursive_scope_resolution ] );
      ( "type",
        [ test_case "type definition" `Quick test_type;
          test_case "duplicate type definition" `Quick test_type_duplicate;
          test_case "duplicate type definition (with a non-type)" `Quick
            test_type_duplicate_non_type;
          test_case "duplicate type field" `Quick test_type_duplicate_field ] );
      ("function", [test_case "function definition" `Quick test_function]) ]
