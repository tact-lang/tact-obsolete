module Lexing' = struct
  let equal_string = String.equal

  let equal_int = Int.equal

  type pos = [%import: Lexing.position] [@@deriving equal]

  open Format

  let pp_pos f p =
    pp_print_string f p.pos_fname ;
    pp_print_string f ":" ;
    pp_print_int f p.pos_lnum ;
    pp_print_string f "," ;
    pp_print_int f p.pos_cnum
end

type pos = Lexing'.pos [@@deriving show {with_path = false}, equal]

(* Z wrapper to enable show derivation *)
module Z' = struct
  type t = [%import: Z.t]

  let pp = Z.pp_print
end

type loc = pos * pos [@@deriving show, equal]

type zt = Z'.t [@@deriving show {with_path = false}]

type 'a located = {loc : loc; value : 'a} [@@deriving show {with_path = false}]

type ident = Ident of string [@@deriving show {with_path = false}]

and type_definition =
  {fields : type_field located list; type_bindings : binding located list}

and interface_definition = {interface_members : binding located list}

and function_call = {fn : expr located; arguments : expr located list}

and enum_definition =
  {enum_members : enum_member located list; enum_bindings : binding located list}

and enum_member = {enum_name : ident located; enum_value : expr located option}

and union_definition =
  {union_members : expr located list; union_bindings : binding located list}

and expr =
  | Let of binding located
  | Type of type_definition
  | Interface of interface_definition
  | Enum of enum_definition
  | Union of union_definition
  | Reference of ident
  | FunctionCall of function_call
  | Function of function_definition
  | Int of zt

and type_field = {field_name : ident located; field_type : expr located}

and function_param = ident located * expr located

and function_definition =
  { name : ident located option;
    params : function_param located list;
    returns : expr located;
    exprs : expr located list option }

and binding = {binding_name : ident located; binding_expr : expr located}

and program = {bindings : binding located list}
[@@deriving show {with_path = false}, make]

let ident_to_string = function Ident s -> s
