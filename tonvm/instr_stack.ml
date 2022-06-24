open Base

type core = [`NOP | `XCHG of int * int | `PUSH of int | `POP of int]
[@@deriving sexp_of, sexp]

type ext = [`SWAP | `XCHG0 of int | `DUP | `OVER | `DROP | `NIP]
[@@deriving sexp_of, sexp]

type t = [core | ext] [@@deriving sexp_of, sexp]

let rec core_of_ext : ext -> core = function
  | `SWAP ->
      core_of_ext (`XCHG0 1)
  | `XCHG0 i ->
      `XCHG (0, i)
  | `DUP ->
      `PUSH 0
  | `OVER ->
      `PUSH 1
  | `DROP ->
      `POP 0
  | `NIP ->
      `POP 1

let execute vm =
  let execute_core vm = function
    | `NOP ->
        vm
    | `XCHG (i, j) ->
        Vm.interchange i j vm
    | `PUSH i ->
        Vm.push (Vm.at vm i) vm
    | `POP i ->
        let _, vm = Vm.interchange 0 i vm |> Vm.pop in
        vm
  in
  function
  | #ext as instr ->
      execute_core vm @@ core_of_ext instr
  | #core as instr ->
      execute_core vm instr
