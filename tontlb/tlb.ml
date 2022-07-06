module Block = Block
module Syntax = Syntax

open struct
  module L = MenhirLib.LexerUtil
end

let parse ?(filename = "noname.tlb") text =
  let lexbuf = L.init filename @@ Lexing.from_string text in
  Parser.root Lexer.token lexbuf
