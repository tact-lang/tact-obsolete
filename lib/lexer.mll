{
  open Parser
  exception Error of string
}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

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
 | "type" { TYPE }
 | "struct" { STRUCT }
 | "enum" { ENUM }
 | "interface" { INTERFACE }
 | "fn" { FN }
 | '-' digit+ as i { INT (int_of_string i) }
 | digit+ as i { INT (int_of_string i) }
 | ident { IDENT (Lexing.lexeme lexbuf) }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
 
