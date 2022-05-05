%token LET INTERFACE TYPE ENUM UNION FN IF ELSE RETURN
%token EQUALS
%token <string> IDENT
%token EOF
%token LBRACE LPAREN
%token RBRACE RPAREN
%token COMMA
%token COLON RARROW SEMICOLON
%token <Z.t> INT

%start <Syntax.program> program

%{ open Syntax %}

%{
  let expand_fn_sugar params loc typ expr =
    Function (make_function_definition ~params: params
                                           ~returns: {loc; value = typ}
                                           ~exprs: [expr]
                                           ())
  %}

%%

(* At the very top, there's a program *)
let program :=
  (* it consists of a list of top-level expressions *)
  top_level = list(top_level_stmt);
  EOF; {
    make_program
       (* collect bindings *)
       ~bindings: (List.filter_map (fun x -> match x with Let(t) -> Some(t) | _ -> None) top_level)
  ()
  }

(* Top level statement *)
let top_level_stmt ==
  (* can be a `let` binding definition *)
  | ~= let_binding; SEMICOLON; <Let>
  | ~= shorthand_binding; <Let>

(* Binding definition

let Name = <expression>

See Expression

There is another "sugared" form of bindings for types and functions:

```
type S{ ... }
type S(T: Type){v: T}
```

They are equivalent to these:

```
let S = type { ... }
let S(T: Type) = type {v: T}
```

Same applies to enums, interfaces, unions and fns

*)
let let_binding ==
| located (
  LET;
  name = located(ident);
  EQUALS;
  expr = located(expr);
  { make_binding ~binding_name: name ~binding_expr: expr () }
)
| located (
  LET;
  name = located(ident);
  params = delimited_separated_trailing_list(LPAREN, function_param, COMMA, RPAREN);
  EQUALS;
  expr = located(expr);
  { make_binding ~binding_name: name
      ~binding_expr:
        { loc = $loc;
          value = expand_fn_sugar params $loc (Reference (Ident "Type")) expr
        }
        ()
  }
)
let shorthand_binding ==
| sugared_function_definition
| located( (name, expr) = type_definition(located(ident)); { make_binding ~binding_name: name ~binding_expr: { loc = $loc; value = expr } () })
| located( ((name, params), expr) = type_definition(located_ident_with_params); {
  make_binding ~binding_name: name ~binding_expr: {
    loc = $loc; value = expand_fn_sugar params $loc (Reference (Ident "Type")) { loc = $loc; value = expr }
  } () })
| located( (name, expr) = interface_definition(located(ident)); { make_binding ~binding_name: name ~binding_expr: { loc = $loc; value = expr } () })
| located( ((name, params), expr) = interface_definition(located_ident_with_params); {
  make_binding ~binding_name: name ~binding_expr: {
    loc = $loc; value = expand_fn_sugar params $loc (Reference (Ident "Interface")) { loc = $loc; value = expr }
    } () })
| located( (name, expr) = enum_definition(located(ident)); { make_binding ~binding_name: name ~binding_expr: { loc = $loc; value = expr } () })
| located( ((name, params), expr) = enum_definition(located_ident_with_params); {
  make_binding ~binding_name: name ~binding_expr: {
    loc = $loc; value = expand_fn_sugar params $loc (Reference (Ident "Type")) { loc = $loc; value = expr }
  } () })
| located( (name, expr) = union_definition(located(ident)); { make_binding ~binding_name: name ~binding_expr: { loc = $loc; value = expr } () })
| located( ((name, params), expr) = union_definition(located_ident_with_params); {
  make_binding ~binding_name: name ~binding_expr: {
    loc = $loc; value = expand_fn_sugar params $loc (Reference (Ident "Type")) { loc = $loc; value = expr }
  } () })


let located_ident_with_params ==
   ~ = located(ident);
   ~ = params;
   <>

let sugared_function_definition ==
   | located( (name, expr) = function_definition(located(ident)); { make_binding ~binding_name: name ~binding_expr: { loc = $loc; value = expr } () })
   | located( ((name, params), expr) = function_definition(located_ident_with_params); {
     make_binding ~binding_name: name ~binding_expr: {
       loc = $loc; value = expand_fn_sugar params $loc (Reference (Ident "Function")) { loc = $loc; value = expr } (* FIXME: Function type is a temp punt *)
       } () })

(* Function definition

fn (arg: Type, ...) -> Type [{
  expr
  expr
  ...
}]

*)
let function_definition(name) :=
  FN;
  n = name;
  params = delimited_separated_trailing_list(LPAREN, function_param, COMMA, RPAREN);
  RARROW;
  returns = located(fexpr);
  body = option(code_block);
  { (n, Function (make_function_definition ~params: params ~returns: returns 
                    ?exprs: (match body with Some(CodeBlock({block_exprs;_})) -> Some(block_exprs) | _ -> None) ())) } 

let function_signature_binding ==
    (n, f) = function_definition(located(ident)); {
    make_binding ~binding_name: n ~binding_expr: { loc = $loc; 
                                                   value = f }
      ()
  }

let function_param ==
  located (
  ~ = located(ident);
  COLON;
  ~ = located(expr);
  <>
  )

(* Function call

   name([argument,])
   <expr>([argument,])

  * Trailing commas are allowed
*)
let function_call :=
  fn = located(fexpr);
  arguments = delimited_separated_trailing_list(LPAREN, located(expr), COMMA, RPAREN);
  { FunctionCall (make_function_call ~fn: fn ~arguments: arguments ()) }

let else_ :=
  | ELSE; ~= if_; <>
  | ELSE; ~= code_block; <>

let if_ :=
  IF;
  condition = delimited(LPAREN, located(expr), RPAREN);
  body = code_block;
  else_ = option(located(else_));
  { If (make_if_ ~condition ~body: (match body with CodeBlock({block_exprs;}) -> block_exprs | _ -> []) ?else_ ()) }

let code_block :=
  block_exprs = delimited(LBRACE, list(located(stmt)), RBRACE);
  {CodeBlock (make_code_block ~block_exprs ())}


(* Statement (they are separated expressions / control flow, but ultimately not very different) *)
let stmt :=
  | terminated(expr, SEMICOLON)
  | ~= terminated(let_binding, SEMICOLON); <Let>
  | if_
  | code_block
  | ~ = delimited(RETURN, expr, SEMICOLON); <Return>

(* Type expression
  
   Difference between type expression and simple expression is that in type expression
   it is not allowed to use exprs with brackets such as `if {}`, `type {}` and `fn() {}`.
 
*)
let type_expr :=
  (* can be any expr delimited by () *)
  | expr = delimited(LPAREN, expr_, RPAREN); {expr}
  (* can be an ident *)
  | ~= ident; <Reference>
  (* can be a function call *)
  | function_call

(* Expression *)
let expr :=
 | expr_
 (* can be a type constructor *)
 | type_constructor
 (* can be a function definition *)
 | (_, f) = function_definition(nothing); { f }

let fexpr :=
 | expr_
  (* can be a type constructor, in parens *)
 | delimited(LPAREN, type_constructor, RPAREN)
 (* can be a function definition, in parens *)
 | (_, f) = delimited(LPAREN, function_definition(nothing), RPAREN); { f }

 let expr_ ==
 (* can be a `type` definition *)
 | (n, s) = type_definition(option(params)); { match n with None -> s | Some(params) -> expand_fn_sugar params $loc (Reference (Ident "Type")) { value = s; loc = $loc} }
  (* can be an `interface` definition *)
 | (_, i) = interface_definition(nothing); { i }
 (* can be an `enum` definition *)
 | (_, e) = enum_definition(nothing); { e }
 (* can be an `union` definition *)
 | (_, u) = union_definition(nothing); { u }
  (* can be an identifier, as a reference to some identifier *)
 | ~= ident; <Reference>
 (* can be a function call *)
 | function_call
 (* can be an integer *)
 | ~= INT; <Int>

let params ==
    delimited_separated_trailing_list(LPAREN, function_param, COMMA, RPAREN)

(* Type

   type {
    field_name: <expression>,
    // or a shorthand version for a type-named field:
    Type,
    ...
    
    fn name(...) -> ... { ... }
    ...
  }

  * Empty types are allowed
  * Trailing commas are allowed

*)
let type_definition(name) ==
  TYPE;
  n = name;
  (fields, bindings) = delimited_separated_trailing_list_followed_by(LBRACE, type_fields, COMMA, list(sugared_function_definition), RBRACE);
  { (n, Type (make_type_definition ~fields: fields ~type_bindings: bindings  ())) }

(* Type field

   field_name: <expression>
   FieldName
*)
let type_fields ==
| located ( name = located(ident); COLON; typ = located(expr); { make_type_field ~field_name: name ~field_type: typ () } )
| located ( name = located(ident); { make_type_field ~field_name: name ~field_type: { loc = name.loc; value = Reference name.value } () } )

(* Type constructor 
 *
 * MyType {
 *   field_name: 1
 * }
 *
 * *)
let type_constructor :=
  constructor_id = located(type_expr);
  fields_construction = delimited_separated_trailing_list(
    LBRACE, 
    separated_pair(
      located(ident), 
      COLON, 
      located(expr)
    ), 
    COMMA, 
    RBRACE);
  {TypeConstructor (make_type_constructor ~constructor_id ~fields_construction ())}

(* Interface

   interface {
     fn name(...) -> Type
   }

*) 

let interface_definition(name) ==
  INTERFACE;
  n = name;
  bindings = delimited(LBRACE, list(located(function_signature_binding)), RBRACE);
  { (n, Interface (make_interface_definition ~interface_members: bindings ())) }

(* Identifier *)
let ident ==
  ~= IDENT ; <Ident>

(* Enum

  enum {
    member,
    member = expr,
    ...
    fn name(...) -> ... { ... }
    ...

  }

  * Empty enums are allowed
  * Trailing commas are allowed
  * exprs must evaluate to integers

*)
let enum_definition(name) ==
  ENUM;
  n = name;
  (members, bindings) = delimited_separated_trailing_list_followed_by(LBRACE, enum_member, COMMA, list(sugared_function_definition), RBRACE);
  { (n, Enum (make_enum_definition ~enum_members: members ~enum_bindings: bindings ())) }

 (* Enum member

    Can be an identifier alone or `identifier = expr` where expr
    must evaluate to integers
*)
let enum_member ==
| located ( name = located(ident); { make_enum_member ~enum_name: name () } )
| located ( name = located(ident); EQUALS; value = located(expr); { make_enum_member ~enum_name: name ~enum_value: value () } )

(* Union

  union {
    member,
    member,
    ...

    fn name(...) -> ... { ... }
    ...

  }

  * Empty unions are allowed
  * Trailing commas are allowed

*)
let union_definition(name) ==
  UNION;
  n = name;
  (members, bindings) = delimited_separated_trailing_list_followed_by(LBRACE, located(union_member), COMMA, list(sugared_function_definition), RBRACE);  
  { (n, Union (make_union_definition ~union_members: members ~union_bindings: bindings ())) }

let union_member :=
 (* can be a type definition *)
 | (_, s) = type_definition(nothing); { s }
 (* can be an `interface` definition *)
 | (_, i) = interface_definition(nothing); { i }
 (* can be an `enum` definition *)
 | (_, e) = enum_definition(nothing); { e }
 (* can be an `union` definition *)
 | (_, u) = union_definition(nothing); { u }
 (* can be an identifier, as a reference to some identifier *)
 | ~= ident; <Reference>
 (* can be a function call [by identifier only] *)
 | fn = located(ident);
  arguments = delimited_separated_trailing_list(LPAREN, located(expr), COMMA, RPAREN);
  { FunctionCall (make_function_call ~fn: { loc = fn.loc; value = (Reference fn.value) } ~arguments: arguments ()) }

(* Delimited list, separated by a separator that may have a trailing separator *)
let delimited_separated_trailing_list(opening, x, sep, closing) ==
 | l = delimited(opening, nonempty_list(terminated(x, sep)), closing); { l }
 | l = delimited(opening, separated_list(sep, x), closing); { l }

(* Delimited list, separated by a separator that may have a trailing separator and followed by something else *)
let delimited_separated_trailing_list_followed_by(opening, x, sep, next, closing) ==
 | opening; ~ = nonempty_list(terminated(x, sep)); ~ = next; closing; <>
 | opening; ~ = separated_list(sep, x); ~ = next; closing; <>

(* Wraps into an `'a located` record *)
let located(x) ==
  ~ = x; { { loc = $loc; value = x } }

let nothing == { None }
