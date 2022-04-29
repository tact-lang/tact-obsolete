{
  open Parser
  exception Error of string
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let integer = digit+
let integer_with_underscores = integer ('_' integer)*

let ident = (alpha) (alpha|digit|'_')* (* regex for identifier *)
let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
 | whitespace { token lexbuf }
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
 | "fn" { FN }
 | '-'? integer_with_underscores as i { INT (Z.of_string i) }
 | ident { IDENT (Lexing.lexeme lexbuf) }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
 
