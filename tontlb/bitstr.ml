include Bitstring
open Base
open Sexplib

let sexp_of_t bits =
  let _, bit_offset, bit_length = bits in
  Sexp.(
    List
      [ Atom
          ( List.range 0 bit_length
          |> List.map ~f:(get bits)
          |> List.map ~f:(function 0 -> "0" | _ -> "1")
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

let equal = equals
