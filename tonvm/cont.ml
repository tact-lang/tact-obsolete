type t = Value.cont

module B = Cell.Builder

let serialize : Cell.builder -> t -> unit =
 fun builder -> function
  | OrdCont {data; code} ->
      let _, _ = (data, code) in
      let%bitstring bits = {| 0: 2 |} in
      B.store_bitstring builder bits
  | _ ->
      failwith "TODO"
