type pos = [%import: Lexing.position] [@@deriving show]

type 'a located =
  { loc: pos * pos;
    value: 'a }
  [@@deriving show]

type ident = Ident of string [@@deriving show]

type struct_field = {
  name: ident located;
  typ: ident located;
} [@@deriving show, make]

type struct_definition = {
  name: ident located;
  fields: struct_field located list;
} [@@deriving show, make]

type interface_definition = {
  name: ident located;
} [@@deriving show, make]

type top_level_expr = 
  | Struct of struct_definition located
  | Interface of interface_definition located
  [@@deriving show]

type program = {
    structs: (struct_definition located) list;
    interfaces: (interface_definition located) list;
  }
  [@@deriving show, make]
