open Base

type instr =
  | ADD
  | NOP
  | SWAP
  | XCHG0 of int
  | PUSHINT of int
  | NEWC
  | STIX
  | ENDC
[@@deriving equal, sexp_of]

class ['s] map =
  object (_ : 's)
    method visit_instr : 'env. 'env -> instr -> instr = fun _env instr -> instr
  end
