(* Workaround for raise Exit in ppx_bitstring + Base *)
exception Exit = Caml.Exit

type t = Bitstring.t

let sexp_of_t _ = Sexplib.Sexp.Atom "<bits>"
