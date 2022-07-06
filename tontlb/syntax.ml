open Base

type tag = Bitstr.t

and ident = string [@@deriving sexp_of]

type field_type = [`Type | `Uint of int]

and type_ = [`Uint of int] [@@deriving sexp_of]

type root = {decls : combinator_decl list}

and combinator_decl =
  { combinator_exotic : bool; [@default false]
    combinator_constructor : constructor;
    combinator : combinator;
    combinator_fields : field list;
    combinator_params : type_expression list }

and combinator = {combinator_ident : ident}

and constructor =
  | AnonymousConstructor of tag option [@sexp.option]
  | Constructor of ident
  | TaggedConstructor of (ident * tag)

and field =
  | ImplicitField of {field_ident : ident; field_type : field_type}
  | ExprField of expr
  | NamedField of {field_ident : ident; field_expr : expr0}
  | AnonymousField of {field_expr : expr0}

and expr =
  | Equals of (expr * expr)
  | Less of (expr * expr)
  | Leq of (expr * expr)
  | Greater of (expr * expr)
  | Geq of (expr * expr)
  | Plus of (expr * expr)
  | Times of (expr * expr)
  | Exprs of expr0 list

and expr0 = type_expression * type_expression option * type_expression option

and type_expression =
  | Runtime of type_expression
  | Expr of expr
  | AnonymousConstr of field list
  | CellRef of type_expression
  | Int of Zint.t
  | NamedRef of ident
  | NatLess
  | NatLeq
  | Type of type_
  | UIntN
[@@deriving make, sexp_of]
