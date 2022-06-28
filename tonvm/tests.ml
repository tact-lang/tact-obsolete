open Base
open Sexplib

let execute program =
  let instrs = List.t_of_sexp Instr.t_of_sexp @@ Sexp.of_string program in
  Vm.execute (Vm.make ()) Instr.execute instrs

let print_stack (vm : Vm.t) =
  List.sexp_of_t Vm.sexp_of_value vm.stack
  |> Sexplib.Sexp.pp_hum Caml.Format.std_formatter

let encode_decode ?(cp = Cp.Codepage 0) instr =
  Instr.encode ~cp instr
  |> Result.bind ~f:(Instr.decode ~cp)
  |> Result.map ~f:fst

let print instr =
  let f = Caml.Format.std_formatter in
  Sexplib.Sexp.pp_hum f
  @@ Result.sexp_of_t Instr.sexp_of_t Instr.sexp_of_error instr ;
  Caml.Format.pp_print_newline f ()
