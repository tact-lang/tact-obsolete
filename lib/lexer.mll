{
  open Tokens
  open Lexing
  exception Error of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }

(* Lexing string literals, approach lifted from OCaml's lexer *)

let empty_str_buffer () = Buffer.create 256
let str_buffer = ref (empty_str_buffer ())

let rec reset_str_buffer () =
  str_buffer := empty_str_buffer () ;
and get_str_buffer () =
  let s = Buffer.contents !str_buffer in
    reset_str_buffer () ;
    s

let store_str_char c =
  Buffer.add_char !str_buffer c

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
 | ';' { SEMICOLON }
 | "->" { RARROW }
 | '{' { LBRACE }
 | '}' { RBRACE }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | '=' { EQUALS }
 | '~' { TILDE }
 | '.' { DOT }
 | '"' { 
     reset_str_buffer ();
     string lexbuf ;
     STRING (get_str_buffer ())
   }
 | "true" { BOOL true }
 | "false" { BOOL false }
 | "let" { LET }
 | "struct" { STRUCT }
 | "enum" { ENUM }
 | "interface" { INTERFACE }
 | "union" { UNION }
 | "fn" { FN }
 | "if" { IF }
 | "else" { ELSE }
 | "return" { RETURN }
 | "val" { VAL }
 | "case" { CASE }
 | '-'? integer_with_underscores as i { INT (Z.of_string i) }
 | ident { IDENT (Lexing.lexeme lexbuf) }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and string = parse
 | '"' { () }
(* string lexer does not currently support any escaping for simplicity's sake,
 * but is expected to have it *)
 | eof { raise (Error "Unterminated string literal") }
 | _ { store_str_char @@ Lexing.lexeme_char lexbuf 0 ;
       string lexbuf }
