open Base

type t =
  [ `NOP
  | `SWAP
  | `XCHG0 of int
  | `XCHG of int * int
  | `PUSH of int
  | `DUP
  | `OVER
  | `POP of int
  | `DROP
  | `NIP ]
[@@deriving sexp_of, sexp]

let execute vm = function
  | `NOP ->
      vm
  | `SWAP ->
      let y, vm = Vm.pop vm in
      let x, vm = Vm.pop vm in
      Vm.push y vm |> Vm.push x
  | `XCHG0 i ->
      Vm.interchange 0 i vm
  | `XCHG (i, j) ->
      Vm.interchange i j vm
  | `PUSH i ->
      Vm.push (Vm.at vm i) vm
  | `DUP ->
      Vm.push (Vm.at vm 0) vm
  | `OVER ->
      Vm.push (Vm.at vm 1) vm
  | `POP i ->
      let _, vm = Vm.interchange 0 i vm |> Vm.pop in
      vm
  | `DROP ->
      let _, vm = Vm.pop vm in
      vm
  | `NIP ->
      let _, vm = Vm.interchange 0 1 vm |> Vm.pop in
      vm
