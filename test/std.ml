module Config = struct
  include Tact.Located.Disabled
end

module Syntax = Tact.Syntax.Make (Config)
module Parser = Tact.Parser.Make (Config)
module Lang = Tact.Lang.Make (Config)
module Show = Tact.Show.Make (Config)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
module C = Tact.Compiler
include Core

type error = [Lang.error | Interpreter.error] * Lang.program
[@@deriving sexp_of]

let make_errors e = new Errors.errors e

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

let compile_std ?(errors = make_errors Show.show_error)
    ?(prev_program = Lang.default_program ()) () =
  let std =
    let c = new Lang.constructor ~program:prev_program errors in
    let p' = c#visit_program () (parse_program Tact.Builtin.std) in
    p'
  in
  (errors, std)

let%test "test std build" =
  Alcotest.(check bool)
    "std build" true
    (let errors, std = compile_std () in
     pp_sexp (sexp_of_string errors#show_errors) ;
     pp_sexp (Lang.sexp_of_program std) ;
     Result.is_ok (errors#to_result ()) )
