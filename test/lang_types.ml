module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
open Lang
open Core

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program stx =
  let elist = make_error_list ~warnings:(ref []) ~errors:(ref []) () in
  let env = env_from_program stx elist in
  match (!(elist.errors), !(elist.warnings)) with
  | [], _ ->
      Ok env
  | errors :: _, _ ->
      Error errors

let pp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

exception Exn of Lang.error

let compile s =
  let env =
    parse_program s |> build_program
    |> Result.map_error ~f:(fun err -> Exn err)
    |> Result.ok_exn
  in
  let env = (new Lang.resolved_references_stripper env)#visit_env () env in
  Lang.eval_env env

let test_alias () =
  let source = {|
  type T { a: Int257 }
  let T1 = T; 
  |} in
  Alcotest.(check bool)
    "aliased types are the same" true
    (let scope = (compile source).scope in
     let t = Lang.in_amap scope (fun m -> Map.find m "T") |> Option.value_exn
     and t1 =
       Lang.in_amap scope (fun m -> Map.find m "T1") |> Option.value_exn
     in
     pp (Lang.sexp_of_term t) ;
     pp (Lang.sexp_of_term t1) ;
     Lang.equal_term t t1 )

let test_carbon_copy () =
  let source = {|
  type T { a: Int257 }
  type T1 { a: Int257 }
  |} in
  Alcotest.(check bool)
    "carbon copy types are not the same" false
    (let scope = (compile source).scope in
     let t = Lang.in_amap scope (fun m -> Map.find m "T") |> Option.value_exn
     and t1 =
       Lang.in_amap scope (fun m -> Map.find m "T1") |> Option.value_exn
     in
     pp (Lang.sexp_of_term t) ;
     pp (Lang.sexp_of_term t1) ;
     Lang.equal_term t t1 )

let test_parameterized () =
  let source =
    {|
  type T(X: Type) { a: X }
  let T1 = T(Int257);
  let T2 = T(Bool);
  let T3 = T(Int257);
  |}
  in
  Alcotest.(check bool)
    "differently parameterized types are not the same" false
    (let scope = (compile source).scope in
     let t1 = Lang.in_amap scope (fun m -> Map.find m "T1") |> Option.value_exn
     and t2 =
       Lang.in_amap scope (fun m -> Map.find m "T2") |> Option.value_exn
     in
     pp (Lang.sexp_of_term t1) ;
     pp (Lang.sexp_of_term t2) ;
     Lang.equal_term t1 t2 ) ;
  Alcotest.(check bool)
    "equally parameterized types are the same" true
    (let scope = (compile source).scope in
     let t1 = Lang.in_amap scope (fun m -> Map.find m "T1") |> Option.value_exn
     and t3 =
       Lang.in_amap scope (fun m -> Map.find m "T3") |> Option.value_exn
     in
     pp (Lang.sexp_of_term t1) ;
     pp (Lang.sexp_of_term t3) ;
     Lang.equal_term t1 t3 )

let () =
  let open Alcotest in
  run "Lang Types"
    [ ( "equality",
        [ test_case "aliases type" `Quick test_alias;
          test_case "carbon copy (same definition)" `Quick test_carbon_copy;
          test_case "parameterized types" `Quick test_parameterized ] ) ]
