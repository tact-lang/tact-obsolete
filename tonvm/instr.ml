open Base
module Stack = Instr_stack
module Null = Instr_null

type t = [Stack.t | Null.t] [@@deriving sexp_of, sexp]

type error = [Stack.error | Null.error]

let[@warning "-11"] sexp_of_error = function
  | #Stack.error as error ->
      Stack.sexp_of_error error
  | #Null.error as error ->
      Null.sexp_of_error error

let execute vm = function
  | #Stack.t as instr ->
      Stack.execute vm instr
  | #Null.t as instr ->
      Null.execute vm instr

let encode ?(cp = Cp.Codepage 0) = function
  | #Stack.t as instr ->
      Stack.encode cp instr
  | #Null.t as instr ->
      Null.encode cp instr

let decode ?(cp = Cp.Codepage 0) bits =
  let decoders = [Stack.decode; Null.decode] in
  let init : (t * Bitstring.t, error) Result.t = Ok (`NOP, bits) in
  List.fold_until ~init
    ~f:(fun _acc decoder ->
      match decoder cp bits with
      | Ok v ->
          Stop (Ok v)
      | Error (`UnsupportedBitcode _bits) as err ->
          Continue err
      | Error _ as err ->
          Stop err )
    ~finish:(fun acc -> acc)
    decoders

let rec decode_all ?(cp = Cp.Codepage 0) ?(init : t list = []) bits =
  match Bitstring.bitstring_length bits with
  | 0 ->
      Ok (List.rev init, bits)
  | _ -> (
    match decode ~cp bits with
    | Ok (v, bits) ->
        decode_all ~cp bits ~init:(v :: init)
    | Error _ as err ->
        err )
