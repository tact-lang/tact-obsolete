(* Workaround for raise Exit in ppx_bitstring + Base *)
exception Exit = Caml.Exit

type t = Bitstring.t

open Base
open Sexplib

let sexp_of_t (data, bit_offset, bit_length) =
  Sexp.(
    List
      [ Atom
          ( Bytes.to_list data |> List.map ~f:Char.to_int
          |> List.map ~f:Int.Hex.to_string
          |> List.map ~f:(String.chop_prefix_exn ~prefix:"0x")
          |> List.map ~f:(fun s ->
                 if phys_equal 1 @@ String.length s then "0" ^ s else s )
          |> String.concat );
        Atom (Int.to_string bit_offset);
        Atom (Int.to_string bit_length) ] )

let t_of_sexp =
  Sexp.(
    function
    | List [Atom data; Atom bit_offset; Atom bit_length] ->
        ( String.to_list data
          |> List.groupi ~break:(fun i _ _ -> Base.equal (i % 2) 0)
          |> List.map ~f:String.of_char_list
          |> List.map ~f:(fun x -> "0x" ^ x)
          |> List.map ~f:Int.Hex.of_string
          |> List.map ~f:Char.of_int_exn
          |> String.of_char_list |> Bytes.of_string,
          Int.of_string bit_offset,
          Int.of_string bit_length )
    | _ ->
        failwith "Invalid sexp" )

module Buffer = struct
  include Bitstring.Buffer

  open struct
    let parent_sexp_of_t = sexp_of_t

    let parent_t_of_sexp = t_of_sexp
  end

  let sexp_of_t buffer = contents buffer |> parent_sexp_of_t

  let t_of_sexp sexp =
    let bytes, _, length = parent_t_of_sexp sexp in
    let buf = create () in
    add_bits buf bytes length ; buf
end
