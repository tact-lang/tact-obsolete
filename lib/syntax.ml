module Lexing' = struct
  type pos = [%import: Lexing.position]

  open Format

  let pp_pos f p = 
    pp_print_string f p.pos_fname;
    pp_print_string f ":";
    pp_print_int f p.pos_lnum;
    pp_print_string f ",";
    pp_print_int f p.pos_cnum
end
type pos = Lexing'.pos [@@deriving show {with_path=false}]

(* Z wrapper to enable show derivation *)
module Z' = struct
  type t = [%import: Z.t]
  let pp = Z.pp_print
end
type zt = Z'.t [@@deriving show {with_path=false}]

type 'a located =
  { loc: pos * pos;
    value: 'a }
  [@@deriving show {with_path=false}]

type ident = Ident of string [@@deriving show {with_path=false}]

and struct_definition = {
  fields: struct_field located list;
  struct_bindings: binding located list;
}
and interface_definition = {
  interface_members: binding located list;
}
and function_call = {
  fn: expr located;
  arguments: expr located list;
}
and enum_definition = {
  enum_members: enum_member located list;
  enum_bindings: binding located list;
}

and enum_member = {
  enum_name: ident located;
  enum_value: expr located option;
}

and union_definition = {
  union_members: expr located list;
  union_bindings: binding located list;
}
and expr = 
  | Struct of struct_definition
  | Interface of interface_definition
  | Enum of enum_definition
  | Union of union_definition
  | Reference of ident
  | FunctionCall of function_call
  | Function of function_definition
  | Int of zt

and struct_field = {
  field_name: ident located;
  field_type: expr located;
}

and function_param = FunctionParam of ident located * expr located

and function_definition = {
  name: ident located option;
  params: function_param located list;
  returns: expr located;
  exprs: expr located list;
}
and type_definition = {
  type_name: ident located;
  type_expr: expr located;
}

and binding = {
  binding_name: ident located;
  binding_expr: expr located;
}

and top_level_expr = 
  | Let of binding located

and program = {
    bindings: (binding located) list;
} 
[@@deriving show {with_path=false}, make]

let ident_to_string i = match i with Ident(s) -> s
