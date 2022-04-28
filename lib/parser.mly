%token LET INTERFACE STRUCT ENUM FN
%token EQUALS
%token <string> IDENT
%token EOF
%token LBRACKET LPAREN
%token RBRACKET RPAREN
%token COMMA
%token COLON
%token <Z.t> INT

%start <Syntax.program> program

%{ open Syntax %}

%%

(* At the very top, there's a program *) 
let program :=
  (* it consists of a list of top-level expressions *)
  top_level = list(top_level_expr);
  EOF; { 
    make_program 
       (* collect bindings *)
       ~bindings: (List.filter_map (fun x -> match x with Let(t) -> Some(t)) top_level) 
  ()
  }

(* Top level expression *)
let top_level_expr ==
  (* can be a `let` binding definition *)
  | ~= binding ; <Let>

(* Binding definition

let Name = <expression> 

See Expression

*)
let binding ==
| located (
  LET;
  name = located(ident);
  EQUALS;
  expr = located(expr);
  { make_binding ~name: name ~expr: expr () }
)
| located (
  LET;
  name = located(ident);
  params = delimited_separated_trailing_list(LPAREN, function_param, COMMA, RPAREN);
  EQUALS;
  expr = located(expr);
  { make_binding ~name: name 
      ~expr: 
        { loc = $loc; 
          value = Function (make_function_definition ~params: params  
                                           ~returns: {loc = $loc; value = Reference (Ident "Type")}
                                           ~exprs: [expr] 
                                           ())
        } 
        () 
  }
)

(* Function definition

fn (arg: Type, ...) Type {
  expr
  expr
  ...
}

*)
let function_definition ==
| 
  FN;
  params = delimited_separated_trailing_list(LPAREN, function_param, COMMA, RPAREN);
  returns = located(expr);
  exprs = delimited(LBRACKET, list(located(expr)), RBRACKET);
  { Function (make_function_definition ~params: params ~returns: returns ~exprs: exprs ()) }

let function_param ==
  located (
  ~ = located(ident);
  COLON;
  ~ = located(expr);
  <FunctionParam>
  )

(* Function call

   name([argument,])
   <expr>([argument,])

  * Trailing commas are allowed
*)
let function_call ==
  fn = located(expr);
  arguments = delimited_separated_trailing_list(LPAREN, located(expr), COMMA, RPAREN);
  { FunctionCall (make_function_call ~fn: fn ~arguments: arguments ()) }


(* Expression *)
let expr :=
 (* can be a `struct` definition *)
 | struct_definition
 (* can be an `interface` definition *)
 | interface_definition
 (* can be an `enum` definition *)
 | enum_definition
 (* can be an identifier, as a reference to some identifier *)
 | ~= ident; <Reference>
 (* can be a function call *)
 | function_call
 (* can be a function definition *)
 | function_definition
 (* can be an integer *)
 | ~= INT; <Int>

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
  { Interface (make_interface_definition ~interface_members: [] ()) }

(* Identifier *)
let ident ==
  ~= IDENT ; <Ident>

(* Enum 

  enum {
    member,
    member = expr,
    ...
  }

  * Empty enums are allowed
  * Trailing commas are allowed
  * exprs must evaluate to integers

*)
let enum_definition ==
| ENUM; 
  members = delimited_separated_trailing_list(LBRACKET, enum_member, COMMA, RBRACKET);
  { Enum (make_enum_definition ~enum_members: members ()) }

 (* Enum member

    Can be an identifier alone or `identifier = expr` where expr
    must evaluate to integers
*)
let enum_member ==
| located ( name = located(ident); { make_enum_member ~enum_name: name () } )
| located ( name = located(ident); EQUALS; value = located(expr); { make_enum_member ~enum_name: name ~enum_value: value () } )

(* Delimited list, separated by a separator that may have a trailing separator *)
let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l } 
 | l = delimited(opening, separated_list(sep, x), closing); { l }

(* Wraps into an `'a located` record *)
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }
 
