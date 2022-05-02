open Base
open Tact.Lang

let parse_program s =
  Tact.Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program = Tact.Lang.env_from_program

let test_scope_resolution () =
  let source = {|
  let I = Int257
  let I_ = I
  let n = 1
  let n_ = n
  |} in
  Alcotest.(check bool)
    "reference resolution" true
    ( match parse_program source |> build_program with
    | Ok {scope; _} ->
        [%matches? Some (Builtin "Int257")] (Map.find scope "I")
        && [%matches? Some (Builtin "Int257")] (Map.find scope "I_")
        && [%matches? Some (Integer _)] (Map.find scope "n")
        && [%matches? Some (Integer _)] (Map.find scope "n_")
    | _ ->
        false )

let test_struct () =
  let source =
    {|
  let Struct = struct {
       a: Int257,
       b: Bool
  }
  |}
  in
  Alcotest.(check bool)
    "struct binding" true
    ( match parse_program source |> build_program with
    | Ok {scope; _} -> (
      match Map.find scope "Struct" with
      | Some (Struct s) ->
          [%matches?
            Some
              {field_type = Resolved_Reference ("Int257", Builtin "Int257"); _}]
            (Map.find s.struct_fields "a")
          && [%matches?
               Some {field_type = Resolved_Reference ("Bool", Builtin "Bool"); _}]
               (Map.find s.struct_fields "b")
      | _ ->
          false )
    | _ ->
        false )

let test_struct_duplicate () =
  let source = {|
  let Struct = struct {}
  let Struct = struct {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ([%matches? Error (Duplicate_Identifier ("Struct", Struct _))]
       (parse_program source |> build_program) )

let test_struct_duplicate_non_struct () =
  let source = {|
  let Struct = 1
  let Struct = struct {}
  |} in
  Alcotest.(check bool)
    "struct binding" true
    ([%matches? Error (Duplicate_Identifier ("Struct", Integer _))]
       (parse_program source |> build_program) )

let test_struct_duplicate_field () =
  let source =
    {|
  let Struct = struct {
      a: Int257,
      a: Bool
  }
  |}
  in
  Alcotest.(check bool)
    "struct binding" true
    ([%matches? Error (Duplicate_Field ("a", _))]
       (parse_program source |> build_program) )

let () =
  let open Alcotest in
  run "Lang"
    [ ( "identifiers",
        [test_case "name resolution in the scope" `Quick test_scope_resolution]
      );
      ( "struct",
        [ test_case "struct definition" `Quick test_struct;
          test_case "duplicate struct definition" `Quick test_struct_duplicate;
          test_case "duplicate struct definition (with a non-structure)" `Quick
            test_struct_duplicate_non_struct;
          test_case "duplicate struct field" `Quick test_struct_duplicate_field
        ] ) ]
