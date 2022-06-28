open Base
open Cp
open Bitstr

type t = [`NULL | `ISNULL] [@@deriving sexp_of, sexp]

type error = [`UnsupportedCodepage of Int.t | `UnsupportedBitcode of Bitstr.t]
[@@deriving sexp_of]

let execute vm = function
  | `NULL ->
      Vm.push Null vm
  | `ISNULL -> (
      let value, vm = Vm.pop vm in
      match value with
      | Null ->
          Vm.push Vm.const_true vm
      | _ ->
          Vm.push Vm.const_false vm )

let encode cp instr =
  match cp with
  | Codepage 0 -> (
    match instr with
    | `NULL ->
        let%bitstring bits = {| 0x6D: 8 |} in
        Ok bits
    | `ISNULL ->
        let%bitstring bits = {| 0x6E: 8 |} in
        Ok bits )
  | Codepage cp ->
      Error (`UnsupportedCodepage cp)

let decode cp bits =
  match cp with
  | Codepage 0 -> (
      match%bitstring bits with
      | {| 0x6D : 8 ; rest : -1 :bitstring |} ->
          Ok (`NULL, rest)
      | {| 0x6E : 8 ; rest : -1 :bitstring |} ->
          Ok (`ISNULL, rest)
      | {|_|} as bits ->
          Error (`UnsupportedBitcode bits) )
  | Codepage cp ->
      Error (`UnsupportedCodepage cp)
