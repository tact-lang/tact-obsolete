include Z

let sexp_of_t z = Sexplib.Sexp.of_string @@ Z.to_string z

let t_of_sexp s = Z.of_string @@ Sexplib.Sexp.to_string s
