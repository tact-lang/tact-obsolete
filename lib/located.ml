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

type span_concrete = pos * pos [@@deriving show, equal, sexp_of]

let merge_spans_concrete : span_concrete -> span_concrete -> span_concrete =
 fun (s1, _) (_, e2) -> (s1, e2)

let merge_spans_concrete_list list =
  let open Base in
  let hd = List.hd_exn list in
  let tl = List.tl_exn list in
  let rec merge_spans_list_inner left = function
    | [] ->
        left
    | x :: xs ->
        merge_spans_list_inner (merge_spans_concrete left x) xs
  in
  merge_spans_list_inner hd tl

module type T = sig
  type span

  type 'a located = {span : span; value : 'a}

  val pp_located :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a located -> unit

  val make_located : span:span_concrete -> value:'a -> unit -> 'a located

  val equal_located : ('a -> 'a -> bool) -> 'a located -> 'a located -> bool

  val hash_fold_located :
    (Base_internalhash_types.state -> 'a -> Base_internalhash_types.state) ->
    Base_internalhash_types.state ->
    'a located ->
    Base_internalhash_types.state

  val compare_located : ('a -> 'a -> int) -> 'a located -> 'a located -> int

  val sexp_of_located : ('a -> Sexplib0.Sexp.t) -> 'a located -> Sexplib0.Sexp.t

  val value : 'a located -> 'a

  val span : 'a located -> span

  val pp_span : Format.formatter -> span -> unit

  val make_loc : 'a 'b. f:('a -> 'b) -> 'a located -> 'b located

  val builtin_located : 'a -> 'a located

  val merge_spans : span -> span -> span

  val merge_spans_list : span list -> span

  val span_of_concrete : span_concrete -> span
end

module Enabled : T = struct
  type span = span_concrete

  type 'a located = {span : span_concrete; [@hash.ignore] value : 'a}
  [@@deriving show {with_path = false}, make, sexp_of, hash]

  let equal_located f {value = value1; _} {value = value2; _} = f value1 value2

  let compare_located f {value = value1; _} {value = value2; _} =
    f value1 value2

  let value l = l.value

  let span l = l.span

  let pp_span = pp_span_concrete

  let make_loc ~f l = {span = l.span; value = f l.value}

  let builtin_located a =
    {span = (Lexing'.dummy_pos, Lexing'.dummy_pos); value = a}

  let merge_spans = merge_spans_concrete

  let merge_spans_list = merge_spans_concrete_list

  let span_of_concrete s = s
end

module Disabled : T = struct
  open Base.Hash.Builtin

  type span = unit

  type 'a located = {span : unit; value : 'a} [@@deriving show, hash]

  let sexp_of_located f l = f l.value

  let make_located ~span:_span ~value () = {span = (); value}

  let equal_located f v1 v2 = f v1.value v2.value

  let compare_located f v1 v2 = f v1.value v2.value

  let value v = v.value

  let span _ = ()

  let pp_span _ _ = ()

  let make_loc ~f l = {span = l.span; value = f l.value}

  let builtin_located a = {span = (); value = a}

  let merge_spans _ _ = ()

  let merge_spans_list _ = ()

  let span_of_concrete _ = ()
end
