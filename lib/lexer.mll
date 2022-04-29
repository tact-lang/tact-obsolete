{
  open Parser
  open Lexing
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }

}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let integer = digit+
let integer_with_underscores = integer ('_' integer)*

let ident = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

rule token = parse
 | whitespace { token lexbuf }
 | newline  { next_line lexbuf; token lexbuf }
 | ',' { COMMA }
 | ':' { COLON }
 | '{' { LBRACKET }
 | '}' { RBRACKET }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | '=' { EQUALS }
 | "let" { LET }
 | "struct" { STRUCT }
 | "enum" { ENUM }
 | "interface" { INTERFACE }
 | "union" { UNION }
 | "fn" { FN }
 | '-'? integer_with_underscores as i { INT (Z.of_string i) }
 | ident { IDENT (Lexing.lexeme lexbuf) }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
 
