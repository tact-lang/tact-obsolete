include Z

let pp = Z.pp_print

let sexp_of_t z = Sexplib.Sexp.of_string (Z.to_string z)

class ['s] map =
  object (_ : 's)
    method visit_z : 'env. 'env -> t -> t = fun _env z -> z
  end

let equal = Z.equal
