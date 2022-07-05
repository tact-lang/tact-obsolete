open Base

module Builder = struct
  type t = {builder_bits : Bitstr.Buffer.t; cells : t list}
  [@@deriving sexp_of, sexp]

  let create () = {builder_bits = Bitstr.Buffer.create (); cells = []}

  let store_bitstring b bits =
    let _bytes, offset, length = bits in
    let bytes, _, _ = Bitstring.subbitstring bits offset length in
    Bitstr.Buffer.add_bits b.builder_bits bytes length

  let add_bit b =
    Bitstr.Buffer.add_bit b.builder_bits
end

type t = {bits : Bitstr.t; refs : t option * t option * t option * t option}

and slice = {slice_cell : t; slice_bits_pos : int; slice_refs_pos : int}

and builder = Builder.t [@@deriving sexp_of, sexp]

let to_slice cell = {slice_cell = cell; slice_bits_pos = 0; slice_refs_pos = 0}

let build (b : builder) =
  {bits = Bitstr.Buffer.contents b.builder_bits; refs = (None, None, None, None)}
