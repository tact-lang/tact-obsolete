open Bitstr
open Sexplib

let%expect_test "Bitstring sexp_of_t" =
  let%bitstring bits = {| 1: 4; 2: 4; 100: 16|} in
  Sexp.pp_hum Caml.Format.std_formatter @@ sexp_of_t bits ;
  [%expect {| (120064 0 24) |}]

let%expect_test "Bitstring t_of_sexp" =
  let%bitstring bits = {| 1: 4; 2: 4; 100: 16|} in
  Sexp.pp_hum Caml.Format.std_formatter
  @@ sexp_of_t @@ t_of_sexp @@ sexp_of_t bits ;
  [%expect {| (120064 0 24) |}]
