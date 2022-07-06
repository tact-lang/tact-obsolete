include Z

(*let pp = Z.pp_print *)

let sexp_of_t z = Sexplib.Sexp.of_string (Z.to_string z)
