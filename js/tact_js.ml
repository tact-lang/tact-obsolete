open Js_of_ocaml
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Errors = Tact.Errors
module CG = Tact.Codegen_func
module Func = Tact.Func
open Errors

let _ =
  Js.export "Tact"
    (object%js (_self)
       method parse (src : Js.js_string Js.t) =
         let src = Js.to_string src in
         let lexbuf = Lexing.from_string src in
         let program =
           let e = new errors in
           let p = Parser.program Tact.Lexer.token lexbuf in
           let c =
             new Lang.constructor Lang.default_bindings Lang.default_methods e
           in
           let p' = c#visit_program () p in
           e#to_result p'
         in
         match program with
         | Ok program ->
             (* Lang.yojson_of_program result |> Yojson.Safe.to_string |> Js.string *)
             let generated = CG.codegen program in
             let buffer = Buffer.create 0 in
             Func.pp_program (Caml.Format.formatter_of_buffer buffer) generated ;
             Buffer.contents buffer |> Js.string
         | Error _ ->
             Js.string "Error"
    end )
