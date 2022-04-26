%token STRUCT
%token INTERFACE
%token <string> IDENT
%token EOF
%token LBRACKET
%token RBRACKET
%token COMMA
%token COLON

%start <Syntax.program> program

%{ open Syntax %}

%%

let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
  
let program :=
  top_level = list(top_level_expr);
  EOF; { make_program 
  ~structs: (List.filter_map (fun x -> match x with Struct(s) -> Some(s) | _ -> None) top_level) 
  ~interfaces: (List.filter_map (fun x -> match x with Interface(s) -> Some(s) | _ -> None) top_level) 
  ()
  }

let top_level_expr ==
  | ~= struct_definition ; <Struct>
  | ~= interface_definition ; <Interface>

let struct_definition ==
| located ( 
  STRUCT ; 
  identifier = ident;
  fields = delimited_separated_trailing_list(LBRACKET, struct_fields, COMMA, RBRACKET);
  { make_struct_definition ~name: identifier () ~fields: fields }
)

let struct_fields ==
| located ( name = ident; COLON; typ = ident; { make_struct_field ~name: name ~typ: typ () } )

let interface_definition ==
| located ( 
  INTERFACE ;
  identifier = ident;
  LBRACKET ; RBRACKET ;
  { make_interface_definition ~name: identifier () }
)

let ident ==
  located ( ~= IDENT ; <Ident> )

let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l } 
 | l = delimited(opening, separated_list(sep, x), closing); { l }
