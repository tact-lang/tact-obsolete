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
  expr = located(expr);
  { make_type_definition ~name: name ~expr: expr () }
)

let expr ==
 | struct_definition
 | interface_definition
 | ~= raw_ident; <Reference>

let struct_definition ==
| STRUCT; 
  fields = delimited_separated_trailing_list(LBRACKET, struct_fields, COMMA, RBRACKET);
  { Struct (make_struct_definition ~fields: fields ()) }

let struct_fields ==
| located ( name = ident; COLON; typ = located(expr); { make_struct_field ~field_name: name ~field_type: typ () } )

let interface_definition ==
|
  INTERFACE;
  LBRACKET ; RBRACKET ;
  { Interface (make_interface_definition ~members: [] ()) }

let ident ==
  located ( raw_ident )

let raw_ident ==
  ~= IDENT ; <Ident>

let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l } 
 | l = delimited(opening, separated_list(sep, x), closing); { l }
