open Base

exception StackUnderflow of int

type cont = int [@@deriving sexp_of, sexp]

type value =
  | Integer of Zint.t
  | Cell
  | Tuple
  | Null
  | Slice
  | Builder
  | Cont of cont
[@@deriving sexp_of, sexp]

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
    c0 : value;
    c1 : value;
    c2 : value;
    c3 : value;
    c4 : value;
    c5 : value;
    c6 : value;
    c7 : value;
    c8 : value;
    c9 : value;
    c10 : value;
    c11 : value;
    c12 : value;
    c13 : value;
    c14 : value;
    c15 : value;
    (* Current continuation *)
    cc : cont;
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

let const_true = Integer (Z.of_int (-1))

and const_false = Integer Z.zero

let make ?(gas = default_gas_limits) () =
  { stack = [];
    c0 = Null;
    c1 = Null;
    c2 = Null;
    c3 = Null;
    c4 = Null;
    c5 = Null;
    c6 = Null;
    c7 = Null;
    c8 = Null;
    c9 = Null;
    c10 = Null;
    c11 = Null;
    c12 = Null;
    c13 = Null;
    c14 = Null;
    c15 = Null;
    cc = 0;
    cp = Stdint.Int16.zero;
    gas }

let execute (vm : t) f instrs = List.fold instrs ~init:vm ~f
