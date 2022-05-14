module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
open Core

type error = [Lang.error | Interpreter.error] [@@deriving sexp_of]

let make_errors () = new Errors.errors

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors ()) ?(bindings = Lang.default_bindings)
    p =
  let c = new Lang.constructor (bindings, errors) in
  let p' = c#visit_program () p in
  errors#to_result p'
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> err) )

let pp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

exception Exn of error list

let compile s =
  parse_program s |> build_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn

let test_equality () =
  let source =
    {|
  let T = Int(257);
  let T1 = Int(257);
  let T2 = Int(256);
  |}
  in
  Alcotest.(check bool)
    "types with same bits are equal" true
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t1 =
       List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     in
     pp (Lang.sexp_of_expr t) ;
     pp (Lang.sexp_of_expr t1) ;
     Lang.equal_expr t t1 ) ;
  Alcotest.(check bool)
    "types with different bits are not equal" false
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t2 =
       List.Assoc.find scope ~equal:String.equal "T2" |> Option.value_exn
     in
     pp (Lang.sexp_of_expr t) ;
     pp (Lang.sexp_of_expr t2) ;
     Lang.equal_expr t t2 )

let () =
  let open Alcotest in
  run "Builtin" [("Int type", [test_case "equality" `Quick test_equality])]
