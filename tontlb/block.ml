let tlb = [%blob "block.tlb"]

(*
unit$_ = Unit;

bit$_ (## 1) = Bit;

hm_edge#_ {n:#} {X:Type} {l:#} {m:#} label:(HmLabel ~l n) 
          {n = (~m) + l} node:(HashmapNode m X) = Hashmap n X;

hmn_leaf#_ {X:Type} value:X = HashmapNode 0 X;
hmn_fork#_ {n:#} {X:Type} left:^(Hashmap n X) 
           right:^(Hashmap n X) = HashmapNode (n + 1) X;

hml_short$0 {m:#} {n:#} len:(Unary ~n) {n <= m} s:(n * Bit) = HmLabel ~n m;
hml_long$10 {m:#} n:(#<= m) s:(n * Bit) = HmLabel ~n m;
hml_same$11 {m:#} v:Bit n:(#<= m) = HmLabel ~n m;

unary_zero$0 = Unary ~0;
unary_succ$1 {n:#} x:(Unary ~n) = Unary ~(n + 1);

hme_empty$0 {n:#} {X:Type} = HashmapE n X;
hme_root$1 {n:#} {X:Type} root:^(Hashmap n X) = HashmapE n X;

*)
(*
module Builder = struct
  type t = {bits : Bitstr.Buffer.t; cells : t list ref}

  let create () = {bits = Bitstr.Buffer.create (); cells = ref []}

  let add_cell {cells; _} =
    let cell = create () in
    cells := cell :: !cells ;
    cell

  let store_bitstring {bits = b; _} bits =
    let _bytes, offset, length = bits in
    let bytes, _, _ = Bitstring.subbitstring bits offset length in
    Bitstr.Buffer.add_bits b bytes length
end

module B = Builder

type nat = int

type unit_ = Unit

and bit = Bit of nat

and 'x hashmap = Hm_edge of {label : 'x hm_label; node : 'x hashmap_node}

and 'x hashmap_node =
  | Hmn_leaf of {value : 'x}
  | Hmn_fork of {left : 'x hashmap; right : 'x hashmap}

and 'x hm_label =
  | Hml_short of {len : unary; s : bit list}
  | Hml_long of {n : nat; s : bit list}
  | Hml_same of {v : bit; n : nat}

and unary = Unary_zero | Unary_succ of {x : unary}

and 'x hashmap_e = Hme_empty | Hme_root of {root : 'x hashmap}

let rec encode_hashmap_e b = function
  | Hme_empty ->
      let%bitstring bits = {|0: 1|} in
      B.store_bitstring b bits
  | Hme_root n ->
      let%bitstring bits = {|1: 1|} in
      B.store_bitstring b bits ;
      let cell = B.add_cell b in
      encode_hashmap cell n.root
and encode_hashmap b = function
  | Hm_edge n ->
    encode_hm_label b n.label ;
    encode_hashmap_node b n.node
and encode_hm_label b = function
  | Hm_short n ->
    encode_unary b n.len ;
    encode_list b encode_bit n.s
  | _ -> failwith "TODO"
and encode_hashmap_node b= function
  | _ -> failwith "TODO"
and encode_unary b = function
  | Zero -> 
    let%bitstring bits = {|0:1|} in
    B.store_bitstring b bits
  | Succ n ->
    let%bitstring bits = {|0:1|} in
    B.store_bitstring b bits ;
    encode_unary b n.x
and encode_bit b = function
  | Bit b ->
    let%bitstring bits = {|b:1|} in
    B.store_bitstring b bits

*)

(*hml_short$0 {m:#} {n:#} len:(Unary ~n) {n <= m} s:(n * Bit) = HmLabel ~n m;
  hml_long$10 {m:#} n:(#<= m) s:(n * Bit) = HmLabel ~n m;
  hml_same$11 {m:#} v:Bit n:(#<= m) = HmLabel ~n m;
*)

(*
type nat_0 = Nat_0

and nat_1 = Nat_1

and nat_2 = Nat_2

type _ nat =
  | Add : 'a nat * 'b nat -> ('a, 'b) add nat
  | Nat : 'a -> 'a nat
  | Int : int -> int nat

and ('a, 'b) add = 'a nat * 'b nat

type 'n natleq = Natleq : 'n nat -> 'n natleq

type ('t, 'x) multiple = Multiple : ('t * 'x list) -> ('t, 'x) multiple

type unit_ = Unit

type bit = Bit of int

type ('n, 'x, 'l, 'm) hashmap =
  | Hm_edge :
      {label : ('l, ('m, 'l) add nat) hm_label; node : (_, 'x, 'm) hashmap_node}
      -> (('m,'l) add nat, 'x, 'l, 'm) hashmap

and ('n, 'x, 'm) hashmap_node =
  | Hmn_leaf : {value : 'x} -> (nat_0 nat, 'x, 'm) hashmap_node
  | Hmn_fork :
      {left : ('n, 'x, _, 'm) hashmap; right : ('n, 'x, _, 'm) hashmap}
      -> (('n, nat_1) add nat, 'x, 'm) hashmap_node

and ('n, 'm) hm_label =
  | Hml_short :
      {len : 'n unary; s : ('n nat, bit) multiple}
      -> ('n nat, 'm) hm_label
  | Hml_long :
      {n : 'm natleq; s : ('n nat, bit) multiple}
      -> ('n nat, 'm) hm_label
  | Hml_same : {v : bit; n : 'm natleq} -> ('n, 'm) hm_label

and _ unary =
  | Nat : 'a nat -> 'a unary
  | Zero : 'a unary
  | Succ : 'a unary -> ('a * 'a) unary
*)

(*

   vm_stack#_ depth:(## 24) stack:(VmStackList depth) = VmStack;

   vm_stk_cons#_ {n:#} rest:^(VmStackList n) tos:VmStackValue = VmStackList (n + 1);
   vm_stk_nil#_ = VmStackList 0;
*)

(*
type vm_stack_list =
  | Vm_stk_nil
  | Vm_stk_cons of {rest: vm_stack_list; tos: vm_stack_value}
and vm_stack = 
  | Vm_stack of {depth: uint24; stack: vm_stack_list}

let rec encode_vm_stack_list b = function
  | Vm_stk_nil -> (* nothing *)
  | Vm_stk_cons n ->
    encode_vm_stack_list (new_cell b) n.rest ;
    encode_vm_stack_value b n.tos

let decode_vm_stack_list n' r = 
  match n' with
  | 0 -> Vm_stk_nil
  (* n' = n + 1 *)
  | _ -> Vm_stk_cons { rest =  decode_vm_stack_list (n' - 1) r; tos = decode_vm_stack_value r }
and
  decode_vm_stack r =
  let depth = decode_uint24 r in
  let stack = decode_vm_stack_list depth r in
  Vm_stack {depth;stack}
*)

(*
   hml_short$0 {m:#} {n:#} len:(Unary ~n) = HmLabel ~n m;

   unary_zero$0 = Unary ~0;
   unary_succ$1 {n:#} x:(Unary ~n) = Unary ~(n + 1);
*)

(*
type unary =
  | Unary_zero
  | Unary_succ of {x: unary}

let rec encode_unary b = function
  | Unary_zero -> add_bit b 0
  | Unary_succ n -> add_bit b 1 ; encode_unary b n.x

let decode_unary r =
  let b = read_bit r in
  match b with
  | 0 -> (Unary_zero, 0)
  | 1 -> let (x,n) = decode_unary r in
    (Unary_succ {x}, n + 1)
   *)
