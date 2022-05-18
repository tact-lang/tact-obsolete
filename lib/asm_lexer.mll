{
  open Asm_parser
  exception Error of string

}

(* Define helper regexes *)
let digit = ['0'-'9']

let integer = digit+

let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

rule token = parse
 | whitespace { token lexbuf }
 | newline { token lexbuf }
 | "NOP" { NOP }
 | "SWAP" { SWAP }
 | "XCHG0" { XCHG0 }
 | "PUSHINT" { PUSHINT }
 | "INT" { PUSHINT }
 | "ADD" { ADD }
 | "NEWC" { NEWC }
 | "STIX" { STIX }
 | "ENDC" { ENDC }
 | integer as i { INT (int_of_string i) }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
