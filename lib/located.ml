module Lexing' = struct
  open Base
  open Caml.Format

  let equal_string = String.equal

  let equal_int = Int.equal

  include Lexing

  let equal_position p1 p2 =
    String.equal p1.pos_fname p2.pos_fname
    && equal p1.pos_lnum p2.pos_lnum
    && equal p1.pos_bol p2.pos_bol
    && equal p1.pos_cnum p2.pos_cnum

  let sexp_of_position _pos = Sexplib.Sexp.(Atom "pos")

  type pos = position [@@deriving equal, sexp_of]

  let pp_pos f p =
    pp_print_string f p.pos_fname ;
    pp_print_string f ":" ;
    pp_print_int f p.pos_lnum ;
    pp_print_string f "," ;
    pp_print_int f p.pos_cnum
end

type pos = Lexing'.pos [@@deriving show {with_path = false}, equal]

let sexp_of_pos = Lexing'.sexp_of_pos

type span = pos * pos [@@deriving show, equal, sexp_of]

module type T = sig
  type 'a located

  val pp_located :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit

  val make_located : span:span -> value:'a -> unit -> 'a located

  val sexp_of_located : ('a -> Sexplib0.Sexp.t) -> 'a located -> Sexplib0.Sexp.t

  val value : 'a located -> 'a

  val span : 'a located -> span
end

module Enabled : T = struct
  type 'a located = {span : span; value : 'a}
  [@@deriving show {with_path = false}, make, sexp_of]

  let value l = l.value

  let span l = l.span
end

module Disabled : T = struct
  type 'a located = 'a [@@deriving show, sexp_of]

  let make_located ~span:_span ~value () = value

  let value v = v

  let span _ = (Lexing.dummy_pos, Lexing.dummy_pos)
end
