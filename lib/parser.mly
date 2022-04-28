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

(* At the very top, there's a program *) 
let program :=
  (* it consists of a list of top-level expressions *)
  top_level = list(top_level_expr);
  EOF; { 
    make_program 
       (* collect defined types *)
       ~types: (List.filter_map (fun x -> match x with Type(t) -> Some(t)) top_level) 
  ()
  }

(* Top level expression *)
let top_level_expr ==
  (* can be a type definition *)
  | ~= type_definition ; <Type>

(* Type definition

type Name = <expression> 

See Expression

*)
let type_definition ==
| located (
  TYPE;
  name = located(ident);
  EQUALS;
  expr = located(expr);
  { make_type_definition ~name: name ~expr: expr () }
)

(* Expression *)
let expr ==
 (* can be a `struct` definition *)
 | struct_definition
 (* can be an `interface` definition *)
 | interface_definition
 (* can be an identifier, as a reference to some identifier *)
 | ~= ident; <Reference>

(* Structure 

  struct {
    field_name: <expression>,
    ...
  }

  * Empty structures are allowed
  * Trailing commas are allowed

*)
let struct_definition ==
| STRUCT; 
  fields = delimited_separated_trailing_list(LBRACKET, struct_fields, COMMA, RBRACKET);
  { Struct (make_struct_definition ~fields: fields ()) }

(* Structure field

   field_name: <expression>
*)
let struct_fields ==
| located ( name = located(ident); COLON; typ = located(expr); { make_struct_field ~field_name: name ~field_type: typ () } )

(* Interface

   interface {
     <interface member>
   }

   NB: member definitions TBD
*)  

let interface_definition ==
|
  INTERFACE;
  LBRACKET ; RBRACKET ;
  { Interface (make_interface_definition ~members: [] ()) }

(* Identifier *)
let ident ==
  ~= IDENT ; <Ident>

(* Delimited list, separated by a separator that may have a trailing separator *)
let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l } 
 | l = delimited(opening, separated_list(sep, x), closing); { l }

(* Wraps into an `'a located` record *)
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
 
