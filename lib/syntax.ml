type pos = [%import: Lexing.position] [@@deriving show]

type 'a located =
  { loc: pos * pos;
    value: 'a }
  [@@deriving show]

type ident = Ident of string [@@deriving show]

and struct_definition = {
  fields: struct_field located list;
} [@@deriving show, make]

and interface_member = {
  member_name: ident located;
} [@@deriving show, make]

and interface_definition = {
  members: interface_member located list;
} [@@deriving show, make]

and function_call = {
  fn: expr located;
  arguments: expr located list;
} [@@deriving show, make]

and expr = 
  | Struct of struct_definition
  | Interface of interface_definition
  | Reference of ident
  | FunctionCall of function_call
  | Function of function_definition
  [@@deriving show]

and struct_field = {
  field_name: ident located;
  field_type: expr located;
} [@@deriving show, make]

and function_param = FunctionParam of ident located * expr located [@@deriving show]

and function_definition = {
  name: ident located option;
  params: function_param located list;
  returns: expr located;
  exprs: expr located list;
} [@@deriving show, make]   

type type_definition = {
  name: ident located;
  expr: expr located;
} [@@deriving show, make]


type top_level_expr = 
  | Type of type_definition located
  | Function of function_definition located
  [@@deriving show]

type program = {
    types: (type_definition located) list;
    functions: (function_definition located) list;
} [@@deriving show, make]
