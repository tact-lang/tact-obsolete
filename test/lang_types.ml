module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module E = Tact.Evaluator.Make (Syntax)
module Errors = Tact.Errors
module Zint = Tact.Zint
open Core

let make_errors () = new Errors.errors

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors ()) ?(bindings = E.default_bindings) p
    =
  let c = new Lang.of_syntax_converter (bindings, errors) in
  let p' = c#visit_program () p in
  let c = new Lang.reference_resolver (p'.bindings, errors) in
  let p' = c#visit_program () p' in
  let p' = (new E.evaluator (p', errors))#visit_program () p' in
  errors#to_result p'
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> err) )

let pp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

exception Exn of Lang.error list

let compile s =
  parse_program s |> build_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn

let test_alias () =
  let source = {|
  struct T { val a: Int257 }
  let T1 = T; 
  |} in
  Alcotest.(check bool)
    "aliased types are the same" true
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t1 =
       List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     in
     pp (Lang.sexp_of_term t) ;
     pp (Lang.sexp_of_term t1) ;
     Lang.equal_term t t1 )

let test_carbon_copy () =
  let source =
    {|
  struct T { val a: Int257 }
  struct T1 { val a: Int257 }
  |}
  in
  Alcotest.(check bool)
    "carbon copy types are not the same" false
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t1 =
       List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     in
     pp (Lang.sexp_of_term t) ;
     pp (Lang.sexp_of_term t1) ;
     Lang.equal_term t t1 )

let test_parameterized () =
  let source =
    {|
  struct T(X: Type) { val a: X }
  let T1 = T(Int257);
  let T2 = T(Bool);
  let T3 = T(Int257);
  |}
  in
  Alcotest.(check bool)
    "differently parameterized types are not the same" false
    (let scope = (compile source).bindings in
     let t1 = List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     and t2 =
       List.Assoc.find scope ~equal:String.equal "T2" |> Option.value_exn
     in
     pp (Lang.sexp_of_term t1) ;
     pp (Lang.sexp_of_term t2) ;
     Lang.equal_term t1 t2 ) ;
  Alcotest.(check bool)
    "equally parameterized types are the same" true
    (let scope = (compile source).bindings in
     let t1 = List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     and t3 =
       List.Assoc.find scope ~equal:String.equal "T3" |> Option.value_exn
     in
     pp (Lang.sexp_of_term t1) ;
     pp (Lang.sexp_of_term t3) ;
     Lang.equal_term t1 t3 )

let () =
  let open Alcotest in
  run "Lang Types"
    [ ( "equality",
        [ test_case "aliased type" `Quick test_alias;
          test_case "carbon copy (same definition)" `Quick test_carbon_copy;
          test_case "parameterized types" `Quick test_parameterized ] ) ]
