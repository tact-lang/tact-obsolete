open Base

exception StackUnderflow of int

type value = Value.t [@@deriving sexp_of, sexp]

type codepage = Stdint.int16

type gas_limits =
  { current_gas_limit : int64;
    maximal_gas_limit : int64;
    remaining_gas : int64;
    gas_credit : int64 }

let default_gas_limits =
  { current_gas_limit = 0L;
    maximal_gas_limit = 0L;
    remaining_gas = 0L;
    gas_credit = 0L }

type t =
  { (* Stack *)
    stack : value list;
    (* Control registers *)
    control_registers : Value.control_registers;
    (* Current continuation *)
    cc : Value.cont option;
    (* Current codepage *)
    cp : codepage;
    (* Gas limits *)
    gas : gas_limits }

let push value vm = {vm with stack = value :: vm.stack}

let pop vm =
  match vm.stack with
  | v :: stack ->
      (v, {vm with stack})
  | [] ->
      raise (StackUnderflow 0)

let at vm x =
  match List.nth vm.stack x with
  | Some x' ->
      x'
  | None ->
      raise (StackUnderflow x)

let interchange x y vm =
  let replace pos a l = List.mapi ~f:(fun i x -> if i = pos then a else x) l in
  match (List.nth vm.stack x, List.nth vm.stack y) with
  | Some x', Some y' ->
      {vm with stack = replace x y' vm.stack |> replace y x'}
  | None, _ ->
      raise (StackUnderflow x)
  | _, None ->
      raise (StackUnderflow y)

let const_true : value = Integer (Z.of_int (-1))

and const_false : value = Integer Z.zero

let make ?(gas = default_gas_limits) () =
  { stack = [];
    control_registers =
      { c0 = None;
        c1 = None;
        c2 = None;
        c3 = None;
        c4 = None;
        c5 = None;
        c7 = None };
    cc = None;
    cp = Stdint.Int16.zero;
    gas }

let execute (vm : t) f instrs = List.fold instrs ~init:vm ~f
