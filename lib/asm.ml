open Base

type instr = ADD | NOP | SWAP | XCHG0 of int
[@@deriving equal, sexp_of, yojson_of]

class ['s] map =
  object (_ : 's)
    method visit_instr : 'env. 'env -> instr -> instr = fun _env instr -> instr
  end
