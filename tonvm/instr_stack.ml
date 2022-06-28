open Base
open Cp
open Bitstr

type core = [`NOP | `XCHG of int * int | `PUSH of int | `POP of int]
[@@deriving sexp_of, sexp]

type ext = [`SWAP | `XCHG0 of int | `DUP | `OVER | `DROP | `NIP]
[@@deriving sexp_of, sexp]

type t = [core | ext] [@@deriving sexp_of, sexp]

type error =
  [ `InvalidInstruction of core
  | `UnsupportedBitcode of Bitstr.t
  | `UnsupportedCodepage of Int.t ]
[@@deriving sexp_of]

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

let encode cp =
  let encode_core instr =
    match cp with
    | Codepage 0 -> (
      match instr with
      | `NOP ->
          let%bitstring bits = {| 0x00: 8 |} in
          Ok bits
      | `XCHG (0, i) when i >= 1 && i <= 15 ->
          let%bitstring bits = {| 0x0: 4; i : 4 |} in
          Ok bits
      | `XCHG (i, j) when i >= 1 && j > i && j <= 15 ->
          let%bitstring bits = {| 0x10: 8; i : 4; j: 4 |} in
          Ok bits
      | `XCHG (0, ii) when ii >= 0 && ii <= 255 ->
          let%bitstring bits = {| 0x11: 8; ii : 8 |} in
          Ok bits
      | `PUSH i when i >= 0 && i <= 15 ->
          let%bitstring bits = {| 0x2: 4; i : 4 |} in
          Ok bits
      | `POP i when i >= 0 && i <= 15 ->
          let%bitstring bits = {| 0x3: 4; i : 4 |} in
          Ok bits
      | instr ->
          Error (`InvalidInstruction instr) )
    | Codepage cp ->
        Error (`UnsupportedCodepage cp)
  in
  function
  | #ext as instr ->
      encode_core @@ core_of_ext instr
  | #core as instr ->
      encode_core instr

let decode cp bits =
  match cp with
  | Codepage 0 -> (
      match%bitstring bits with
      | {| 0x00 : 8 ; rest : -1 : bitstring |} ->
          Ok (`NOP, rest)
      | {| 0x0 : 4 ; i : 4; rest : -1 : bitstring |} ->
          Ok (`XCHG (0, i), rest)
      | {| 0x10 : 8 ; i : 4; j : 4; rest : -1 : bitstring |} ->
          Ok (`XCHG (i, j), rest)
      | {| 0x11 : 8 ; ii : 8; rest : -1 : bitstring |} ->
          Ok (`XCHG (0, ii), rest)
      | {| 0x2 : 4 ; i : 4; rest : -1 : bitstring |} ->
          Ok (`PUSH i, rest)
      | {| 0x3 : 4 ; i : 4; rest : -1 : bitstring |} ->
          Ok (`POP i, rest)
      | {|_|} as bits ->
          Error (`UnsupportedBitcode bits) )
  | Codepage cp ->
      Error (`UnsupportedCodepage cp)
