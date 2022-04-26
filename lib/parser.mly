%token TYPE
%token EQUALS
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
  ~types: (List.filter_map (fun x -> match x with Type(t) -> Some(t)) top_level) 
  ()
  }

let top_level_expr ==
  | ~= type_definition ; <Type>

let type_definition ==
| located (
  TYPE;
  name = ident;
  EQUALS;
  expr = expr;
  { make_type_definition ~name: name ~expr: expr () }
)

let expr ==
 | struct_definition
 | interface_definition

let struct_definition ==
| located ( 
  STRUCT; 
  fields = delimited_separated_trailing_list(LBRACKET, struct_fields, COMMA, RBRACKET);
  { Struct (make_struct_definition ~fields: fields ()) }
)

let struct_fields ==
| located ( name = ident; COLON; typ = ident; { make_struct_field ~name: name ~typ: typ () } )

let interface_definition ==
| located ( 
  INTERFACE;
  LBRACKET ; RBRACKET ;
  { Interface (make_interface_definition ~members: [] ()) }
)

let ident ==
  located ( ~= IDENT ; <Ident> )

let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l } 
 | l = delimited(opening, separated_list(sep, x), closing); { l }
