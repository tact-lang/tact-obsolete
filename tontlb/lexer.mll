{
  open Parser
  open Lexing
  exception Error of string

  (* Line number management *)
  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                 pos_lnum = pos.pos_lnum + 1;
      }

  (* Comment tracking *)
  let comment_ctr = ref 0
  let single_line_comment_flag = ref false

  (* Tag encoding *)
  type tag = Binary | Hex

  let determine_tag_encoding s =
    let length = String.length s
    in match s.[length - 1] with
    | '$' -> Binary
    | '#' -> Hex
    | _ -> raise (Error "unexpected input")

  let bitstring_of_binary b =
    let buf = Bitstring.Buffer.create () in
    Base.List.iter ~f:(function '0' -> Bitstring.Buffer.add_bit buf false 
                      | '1' -> Bitstring.Buffer.add_bit buf true
                      | _ -> raise (Error "invalid binary")) @@ Base.String.to_list_rev b ;
    Bitstring.Buffer.contents buf

  let bitstring_of_hex h =
    let buf = Bitstring.Buffer.create () in
    let hex_to_int hex =
      Base.Int.Hex.of_string @@ "0x" ^ (Base.String.of_char hex)
    in
    let iter =
      Base.List.iter ~f:(fun hex ->
          let i = (hex_to_int hex) in
          let bits = [0b0001;0b0010;0b0100;0b1000] in
          let i' = Base.List.map bits ~f:(fun b -> (i land b) = b) in
          Base.List.iter i' ~f:(Bitstring.Buffer.add_bit buf)) in
    match Base.String.to_list_rev h with
    (* Incomplete *)
    | '_' :: hex -> 
      iter hex ; 
      let bits = Bitstring.Buffer.contents buf in
      let bitlist = Base.List.range 0 (Bitstring.bitstring_length bits) |>
                    Base.List.map ~f:(Bitstring.get bits) |> Base.List.rev in
      let bitlist' = Base.List.drop_while ~f:(Base.phys_equal 0) bitlist in
      let bitlist' = Base.List.drop bitlist' 1 in
      Bitstring.subbitstring bits 0 @@ Base.List.length bitlist'
    (* Complete *)
    | hex -> 
      iter hex;
      Bitstring.Buffer.contents buf

}

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let integer = digit+
let integer_with_underscores = integer ('_' integer)*

let ident = (alpha|'_') (alpha|digit|'_')* (* regex for identifier *)
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

let binary = ['0' '1']+
let hex = ['0'-'9' 'A'-'F' 'a'-'f']+ '_'*

let constructor = ident ('$' | '#')

rule token = parse
 | whitespace { token lexbuf }
 | newline  { next_line lexbuf; token lexbuf }
 | '+' { PLUS }
 | '*' { TIMES }
 | ':' { COLON }
 | ';' { SEMICOLON }
 | '{' { LBRACE }
 | '}' { RBRACE }
 | '(' { LPAREN }
 | ')' { RPAREN }
 | '[' { LBRACKET }
 | ']' { RBRACKET }
 | '=' { EQUALS }
 | '_' { UNDERSCORE }
 | '!' { EXCL }
 | '?' { QUESTION }
 | '.' { DOT }
 | '~' { TILDE }
 | "##" { DOUBLEHASH }
 | '#' { HASH }
 | '^' { CIRCUMFLEX }
 | '<' { LESS }
 | '>' { GREATER }
 | "<=" { LEQ }    
 | ">=" { GEQ }
 | "#<" { NAT_LESS }
 | "#<=" { NAT_LEQ }
 | "Type" { Type }
 | integer as i { INT (Z.of_string i) }
 | constructor as c { match (determine_tag_encoding c) with 
                      | Binary -> binary_constructor (Base.String.chop_suffix_exn c ~suffix:"$") lexbuf 
                      | Hex -> hex_constructor (Base.String.chop_suffix_exn c ~suffix:"#") lexbuf }
 | ident { IDENT (Lexing.lexeme lexbuf) }
 | "/*" { comment_ctr := !comment_ctr + 1 ; comment lexbuf }
 | "//" { single_line_comment_flag := true ; single_line_comment lexbuf }
 | eof { EOF }
 | _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

and comment = parse
 | "/*" { comment_ctr := !comment_ctr + 1 ; comment lexbuf }
 | "*/" { 
     comment_ctr := !comment_ctr - 1 ;
     if !comment_ctr == 0 then
      if !single_line_comment_flag then single_line_comment lexbuf
      else token lexbuf
     else
       comment lexbuf
   }
 | _ { comment lexbuf }

and single_line_comment = parse
 | newline { single_line_comment_flag := false; token lexbuf }
 | _ { single_line_comment lexbuf }

and binary_constructor c = parse
 | '_' { CONSTRUCTOR (c, Bitstring.empty_bitstring) }
 | binary as b { CONSTRUCTOR (c, bitstring_of_binary b) } 
 | _ { token lexbuf }

and hex_constructor c = parse
 | '_' { CONSTRUCTOR (c, Bitstring.empty_bitstring) }
 | hex as h { CONSTRUCTOR (c, bitstring_of_hex h) } 
 | _ { token lexbuf }
