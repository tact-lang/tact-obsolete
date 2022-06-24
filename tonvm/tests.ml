open Base
open Sexplib

let execute program =
  let instrs = List.t_of_sexp Instr.t_of_sexp @@ Sexp.of_string program in
  Vm.execute (Vm.make ()) Instr.execute instrs

let print_stack (vm : Vm.t) =
  List.sexp_of_t Vm.sexp_of_value vm.stack
  |> Sexplib.Sexp.pp_hum Caml.Format.std_formatter
