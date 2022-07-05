open Base
open Bitstr [@warning "-33"] (* allow unused open *)

type cont =
  | OrdCont of {data : control_registers; code : Cell.slice}
  | QuitCont of int
  | ExcQuitCont
  | PushIntCont of {push_val : int; next : cont}
  | RepeatCont of {count : int; body : t; after : cont}
  | AgainCont of {body : cont}
  | UntilCont of {body : cont; after : cont}
  | WhileCont of {cond : cont; body : cont; after : cont; check_cond : bool}
  | ArgContExt of {data : control_registers; ext : cont}

and control_registers =
  { c0 : cont option;
    c1 : cont option;
    c2 : cont option;
    c3 : cont option;
    c4 : cont option;
    c5 : cont option;
    c7 : t list option }

and t =
  | Integer of Zint.t
  | Cell of Cell.t
  | Tuple of t list
  | Null
  | Slice of Cell.slice
  | Builder
  | Cont of cont
[@@deriving sexp_of, sexp]

let encode_t b = function
  | Null ->
    Cell.Builder.store_bitstring b [%bitstring {|0: 8|}]
  | Integer n ->
    Cell.Builder.store_bitstring b [%bitstring {|2: 8; 0:9|}] ; 
    for i = 0 to 257 do
      Cell.Builder.add_bit b (Zint.testbit n i)
    done
  | _ ->
      failwith "TODO"
