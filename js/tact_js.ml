open Js_of_ocaml
open Base

let _ =
  Js.export "Tact"
    (object%js (_self)
       method parse (src : Js.js_string Js.t) =
         let src = Js.to_string src in
         match Tact.Compiler.compile_from_string src with
         | Ok program ->
             Js.string program
         | Error errors ->
             Js.string errors
    end )
