%token PLUS TIMES COLON SEMICOLON LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET EQUALS UNDERSCORE EXCL QUESTION DOT TILDE DOUBLEHASH HASH
%token CIRCUMFLEX LESS GREATER LEQ GEQ NAT_LESS NAT_LEQ 

%token Type
%token <string> IDENT
%token <string * Bitstring.t> CONSTRUCTOR

%token <Z.t> INT

%token EOF

%start <Syntax.root> root

%{

open Syntax

(* Helpers *)

let constructor ident (b: Bitstring.t) = match b with
  | (bytes, _, _) when Bytes.length bytes == 0 -> Constructor ident
  | tag when String.equal ident "_" -> AnonymousConstructor (Some tag)
  | tag -> TaggedConstructor (ident, tag)

%}

%%

let root :=
    | decls = list(combinator_decl); EOF; { make_root ~decls () }

let combinator_decl :=
    | combinator_exotic = bool_option(EXCL); 
      combinator_constructor = constructor; 
      combinator_fields = list(combinator_field);
      EQUALS; combinator = combinator; 
      combinator_params = list(type_expression);
      SEMICOLON; 
      { make_combinator_decl ~combinator_constructor ~combinator ~combinator_exotic ~combinator_fields ~combinator_params ()  }

let constructor :=
    | (ident, tag) = CONSTRUCTOR; { constructor ident tag }
    | ident = IDENT; { Constructor ident }
    | UNDERSCORE; { AnonymousConstructor None }

let combinator :=
    | combinator_ident = IDENT; { make_combinator ~combinator_ident () } 

let combinator_field :=
    | (field_ident, field_type) = delimited(LBRACE, separated_pair(IDENT, COLON, implicit_field_type), RBRACE);
    { ImplicitField {field_ident; field_type} }
    | expr = delimited(LBRACE, expression, RBRACE); { ExprField expr }
    | (field_ident, field_expr) = separated_pair(IDENT, COLON, expr); { NamedField {field_ident; field_expr} }

    | (_, field_expr) = separated_pair(UNDERSCORE, COLON, expr); { AnonymousField {field_expr} }
    | expr = expr; { ExprField expr }

let implicit_field_type :=
    | HASH; { `Uint 32 }
    | Type; { `Type }

let type_expression :=
    | TILDE; expr = type_expression; { Runtime expr }
    | LPAREN ; expr = expression; RPAREN; { Expr expr }
    | LBRACKET; fields = list(combinator_field); RBRACKET; { AnonymousConstr fields }
    | CIRCUMFLEX ; expr = type_expression; { CellRef expr }
    | num = INT; { Int num }
    | ident = IDENT; { NamedRef ident }
    | NAT_LESS; { NatLess }
    | NAT_LEQ; { NatLeq }
    | HASH; { Type (`Uint 32) }
    | DOUBLEHASH; { UIntN }

let expression :=
    | (x, y) = separated_pair(expression_, EQUALS, expression_); { Equals (x,y) }
    | (x, y) = separated_pair(expression_, LEQ, expression_); { Leq (x,y) }
    | (x, y) = separated_pair(expression_, LESS, expression_); { Less (x,y) }
    | (x, y) = separated_pair(expression_, GREATER, expression_); { Greater (x,y) }
    | (x, y) = separated_pair(expression_, GEQ, expression_); { Geq (x,y) }
    | expr = expression_; { expr } 

let expression_ :=
    | (x, y) = separated_pair(expression__, PLUS, expression__); { Plus (x,y) }
    | expr = expression__; { expr }

let expression__ :=
    | (x, y) = separated_pair(expressions, TIMES, expressions); { Times (x,y) } 
    | expr = expressions; { expr }

let expressions :=
    | exprs = list(expr); { Exprs exprs }

let expr :=
    | expr1 = type_expression; expr2 = option(preceded(DOT, type_expression)); cond = option(preceded(QUESTION, type_expression));
    { (expr1, expr2, cond) }


let bool_option(x) == m = option(x); { Option.is_some m } 
