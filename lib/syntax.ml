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
} [@@deriving show {with_path=false}, make]

and interface_member = {
  member_name: ident located;
} [@@deriving show {with_path=false}, make]

and interface_definition = {
  interface_members: interface_member located list;
} [@@deriving show {with_path=false}, make]

and function_call = {
  fn: expr located;
  arguments: expr located list;
} [@@deriving show {with_path=false}, make]

and enum_definition = {
  enum_members: enum_member located list;
} [@@deriving show {with_path=false}, make]

and enum_member = {
  enum_name: ident located;
  enum_value: expr located option;
} [@@deriving show {with_path=false}, make]

and expr = 
  | Struct of struct_definition
  | Interface of interface_definition
  | Enum of enum_definition
  | Reference of ident
  | FunctionCall of function_call
  | Function of function_definition
  | Int of zt
  [@@deriving show {with_path=false}]

and struct_field = {
  field_name: ident located;
  field_type: expr located;
} [@@deriving show {with_path=false}, make]

and function_param = FunctionParam of ident located * expr located [@@deriving show {with_path=false}]

and function_definition = {
  name: ident located option;
  params: function_param located list;
  returns: expr located;
  exprs: expr located list;
} [@@deriving show {with_path=false}, make]   

type type_definition = {
  name: ident located;
  expr: expr located;
} [@@deriving show {with_path=false}, make]

type binding = {
  name: ident located;
  expr: expr located;
} [@@deriving show {with_path=false}, make]

type top_level_expr = 
  | Let of binding located
  [@@deriving show {with_path=false}]

type program = {
    bindings: (binding located) list;
} [@@deriving show {with_path=false}, make]
