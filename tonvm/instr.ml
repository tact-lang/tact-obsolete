module Stack = Instr_stack
module Null = Instr_null

type t = [Stack.t | Null.t] [@@deriving sexp_of, sexp]

let execute vm = function
  | #Stack.t as instr ->
      Stack.execute vm instr
  | #Null.t as instr ->
      Null.execute vm instr
