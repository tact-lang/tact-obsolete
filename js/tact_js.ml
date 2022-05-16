open Js_of_ocaml
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Errors = Tact.Errors
open Errors

let _ =
  Js.export "Tact"
    (object%js (_self)
       method parse (src : Js.js_string Js.t) =
         let src = Js.to_string src in
         let lexbuf = Lexing.from_string src in
         let result =
           let e = new errors in
           let p = Parser.program Tact.Lexer.token lexbuf in
           let c = new Lang.constructor (Lang.default_bindings, e) in
           let p' = c#visit_program () p in
           e#to_result p'
         in
         match result with
         | Ok result ->
             (* FIXME: figure out a better way *)
             Lang.yojson_of_program result |> Yojson.Safe.to_string |> Js.string
         | Error _ ->
             Js.string "Error"
    end )
