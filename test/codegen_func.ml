open Shared.Disabled
module Config = Shared.DisabledConfig

let%expect_test "simple function generation" =
  let source = {|
      fn test() -> Integer { return 0; }
    |} in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    int test() impure {
      return 0;
    } |}]

let%expect_test "passing struct to a function" =
  let source =
    {|
         struct T {
          val b: Integer
          val c: struct { val d : Integer }
         }
         fn test(t: T) -> Integer { return 1; }
       |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int test([int, int] t) impure {
         return 1;
       } |}]

let%expect_test "function calls" =
  let source =
    {|
      fn test(value: Integer) -> Integer { return value; }
      fn test2(value: Integer) -> Integer { return test(value); }
    |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int test(int value) impure {
         return value;
       }
       int test2(int value) impure {
         return test(value);
       } |}]

let%expect_test "Int[bits] serializer codegen" =
  let source =
    {|
      fn test_int(b: Builder) {
        let i = Int[32].new(100);
        i.serialize(b);
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       builder f2(builder self, int int_, int bits) impure {
         return builtin_store_int(self, int_, bits);
       }
       builder f1(int self, builder builder_) impure {
         return f2(builder_, self, 32);
       }
       _ test_int(builder b) impure {
         int i = 100;
         return f1(100, b);
       } |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
      struct T {
        val a: Int[32]
        val b: Int[16]
      }
      let T_serializer = serializer[T];

      fn test() {
        let b = Builder.new();
        T_serializer(T{a: Int[32].new(0), b: Int[16].new(1)}, b);
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       forall A -> A get1(tuple t) asm "1 INDEX";
       forall A -> A get0(tuple t) asm "0 INDEX";
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       builder f2(builder self, int int_, int bits) impure {
         return builtin_store_int(self, int_, bits);
       }
       builder f1(int self, builder builder_) impure {
         return f2(builder_, self, 32);
       }
       builder f3(int self, builder builder_) impure {
         return f2(builder_, self, 16);
       }
       builder T_serializer([int, int] self, builder b) impure {
         builder b = f1(get0(func_believe_me(self)), b);
         builder b = f3(get1(func_believe_me(self)), b);
         return b;
       }
       builder f4() impure {
         return builtin_begin_cell();
       }
       _ test() impure {
         builder b = f4();
         return T_serializer([0, 1], b);
       } |}]

let%expect_test "demo struct serializer 2" =
  let source =
    {|
      struct Foo {
        val a: Int[32]
        val b: Int[16]
      }
      let serialize_foo = serializer[Foo];

      fn test() -> Builder {
        let b = Builder.new();
        return serialize_foo(Foo{a: Int[32].new(0), b: Int[16].new(1)}, b);
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       forall A -> A get1(tuple t) asm "1 INDEX";
       forall A -> A get0(tuple t) asm "0 INDEX";
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       builder f2(builder self, int int_, int bits) impure {
         return builtin_store_int(self, int_, bits);
       }
       builder f1(int self, builder builder_) impure {
         return f2(builder_, self, 32);
       }
       builder f3(int self, builder builder_) impure {
         return f2(builder_, self, 16);
       }
       builder serialize_foo([int, int] self, builder b) impure {
         builder b = f1(get0(func_believe_me(self)), b);
         builder b = f3(get1(func_believe_me(self)), b);
         return b;
       }
       builder f4() impure {
         return builtin_begin_cell();
       }
       builder test() impure {
         builder b = f4();
         return serialize_foo([0, 1], b);
       } |}]

let%expect_test "true and false" =
  let source =
    {|
      fn test(flag: Bool) {
        if (flag) {
          return false;
        } else {
          return true;
        }
      }
    |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    int test(int flag) impure {
      if (flag) {
      return 0;
    } else
    {
      return -1;
    }} |}]

let%expect_test "if/then/else" =
  let source =
    {|
      fn test(flag: Bool) {
        if (flag) {
          return 1;
        } else {
          return 2;
        }
      }
    |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
         forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
           (Value1 value, _) = tensor;
           return value;
         }
         forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
           (_, Value2 value) = tensor;
           return value;
         }
         forall A, B -> B func_believe_me(A i) asm "NOP";
         int func_bit_not(int a) {
           return ~ a;
         }
         int test(int flag) impure {
           if (flag) {
           return 1;
         } else
         {
           return 2;
         }} |}]

let%expect_test "serializer inner struct" =
  let source =
    {|
      struct Pubkey { val x: Int[160] }
      struct Wallet { val seqno: Int[32] val pubkey: Pubkey }
      let serialize_wallet = serializer[Wallet];
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       forall A -> A get0(tuple t) asm "0 INDEX";
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       builder f2(builder self, int int_, int bits) impure {
         return builtin_store_int(self, int_, bits);
       }
       builder f1(int self, builder builder_) impure {
         return f2(builder_, self, 32);
       }
       builder serialize_wallet([int, int] self, builder b) impure {
         builder b = f1(get0(func_believe_me(self)), b);
         return b;
       } |}]

let%expect_test "unions" =
  let source =
    {|
       struct Empty{}
       union Uni {
         case Integer
         case Empty
       }
       fn try(x: Uni) -> Uni { x }
       fn test_try(x: Integer, y: Empty) {
         let test1 = try(x);
         let test2 = try(y);
       }
     |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       tuple try(tuple x) impure {
         return x;
       } |}]

let%expect_test "switch statement" =
  let source =
    {|
         union Ints {
           case Int[32]
           case Int[64]
         }
         fn test(i: Ints) -> Integer {
           switch (i) {
             case Int[32] vax => { return 32; }
             case Int[64] vax => { return 64; }
           }
         }
       |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       int test(tuple i) impure {
         {
         tuple temp = i;
       int discr =
       first(temp);
       if (discr == 0)
       {
         int vax = second(temp);
       {
         return 32;
       }} else if (discr == 1)
       {
         int vax = second(temp);
       {
         return 64;
       }} else
       {
         }}} |}]

let%expect_test "tensor2" =
  let source =
    {|
       fn test() {
         let x = builtin_divmod(10, 2);
         return x.value1;
       }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       int test() impure {
         (int, int) x = builtin_divmod(10, 2);
         return tensor2_value1(x);
       }
      |}]

let%expect_test "serialization api" =
  let source =
    {|
      struct Empty {
        impl Serialize {
          fn serialize(self: Self, b: Builder) -> Builder {
            return b;
          }
        }
      }
      fn test(m: MessageRelaxed[Empty]) {
        let b = Builder.new();
        let b = m.serialize(b);
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       forall A -> A get3(tuple t) asm "3 INDEX";
       forall A -> A get2(tuple t) asm "2 INDEX";
       forall A -> A get1(tuple t) asm "1 INDEX";
       forall A -> A get0(tuple t) asm "0 INDEX";
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       builder f1() impure {
         return builtin_begin_cell();
       }
       builder f4(builder self, int int_, int bits) impure {
         return builtin_store_int(self, int_, bits);
       }
       builder f11([] self, builder b) impure {
         return b;
       }
       builder f13(int self, builder builder_) impure {
         return f4(builder_, self, 9);
       }
       builder f12([int, int] self, builder b) impure {
         builder b = f13(get0(func_believe_me(self)), b);
         builder b = f4(b, get1(func_believe_me(self)), get0(func_believe_me(self)));
         return b;
       }
       builder f10(tuple self, builder b) impure {
         {
         tuple temp = self;
       int discr =
       first(temp);
       if (discr == 1)
       {
         [int, int] var = second(temp);
       {
         _ b = store_uint(b, 1, 1);
       builder b =
       f12(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         [] var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f11(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f9(tuple self, builder b) impure {
         return f10(self, b);
       }
       builder f18(int self, builder builder_) impure {
         return f4(builder_, self, 8);
       }
       builder f19(int self, builder builder_) impure {
         return f4(builder_, self, 256);
       }
       builder f17([int, int] self, builder b) impure {
         builder b = f18(get0(func_believe_me(self)), b);
         builder b = f19(get1(func_believe_me(self)), b);
         return b;
       }
       builder f16([int, int] self, builder b) impure {
         builder b = f4(b, 0, 0);
         return f17(self, b);
       }
       builder f22(int self, builder builder_) impure {
         return f4(builder_, self, 32);
       }
       builder f21([int, int, int] self, builder b) impure {
         builder b = f13(get0(func_believe_me(self)), b);
         builder b = f22(get1(func_believe_me(self)), b);
         return b;
       }
       builder f20([int, int, int] self, builder b) impure {
         builder b = f4(b, 0, 0);
         builder b = f21(self, b);
         return b;
       }
       builder f15(tuple self, builder b) impure {
         {
         tuple temp = self;
       int discr =
       first(temp);
       if (discr == 1)
       {
         [int, int, int] var = second(temp);
       {
         _ b = store_uint(b, 1, 1);
       builder b =
       f20(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         [int, int] var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f16(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f14(tuple self, builder b) impure {
         return f15(self, b);
       }
       builder f8(tuple self, builder b) impure {
         {
         tuple temp = self;
       int discr =
       first(temp);
       if (discr == 1)
       {
         tuple var = second(temp);
       {
         _ b = store_uint(b, 1, 1);
       builder b =
       f14(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         tuple var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f9(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f7(tuple self, builder b) impure {
         return f8(self, b);
       }
       builder f24(builder self, int uint, int bits) impure {
         return builtin_store_uint(self, uint, bits);
       }
       builder f23(int self, builder builder_) impure {
         return f24(builder_, self, 64);
       }
       builder f25(int self, builder builder_) impure {
         return f24(builder_, self, 32);
       }
       builder f6([tuple, tuple, int, int] self, builder b) impure {
         builder b = f7(get0(func_believe_me(self)), b);
         builder b = f9(get1(func_believe_me(self)), b);
         builder b = f23(get2(func_believe_me(self)), b);
         builder b = f25(get3(func_believe_me(self)), b);
         return b;
       }
       builder f5([tuple, tuple, int, int] self, builder b) impure {
         return f6(self, b);
       }
       builder f30(int self, builder builder_) impure {
         return f4(builder_, self, 1);
       }
       builder f29([int, int, int] self, builder b) impure {
         builder b = f30(get0(func_believe_me(self)), b);
         builder b = f30(get1(func_believe_me(self)), b);
         builder b = f30(get2(func_believe_me(self)), b);
         return b;
       }
       builder f28([int, int, int] self, builder b) impure {
         return f29(self, b);
       }
       builder f32([tuple, tuple] self, builder b) impure {
         builder b = f7(get0(func_believe_me(self)), b);
         builder b = f14(get1(func_believe_me(self)), b);
         return b;
       }
       builder f31([tuple, tuple] self, builder b) impure {
         return f32(self, b);
       }
       builder f36(builder self, int c) impure {
         return builtin_store_grams(self, c);
       }
       builder f35(int self, builder builder_) impure {
         return f36(builder_, self);
       }
       builder f34([int, int] self, builder b) impure {
         builder b = f35(get0(func_believe_me(self)), b);
         builder b = f35(get1(func_believe_me(self)), b);
         return b;
       }
       builder f33([int, int] self, builder b) impure {
         return f34(self, b);
       }
       builder f38([int, int] self, builder b) impure {
         builder b = f23(get0(func_believe_me(self)), b);
         builder b = f25(get1(func_believe_me(self)), b);
         return b;
       }
       builder f37([int, int] self, builder b) impure {
         return f38(self, b);
       }
       builder f27([[int, int, int], [tuple, tuple], [int, int], [int, int]]
           self, builder b) impure {
         builder b = f28(get0(func_believe_me(self)), b);
         builder b = f31(get1(func_believe_me(self)), b);
         builder b = f33(get2(func_believe_me(self)), b);
         builder b = f37(get3(func_believe_me(self)), b);
         return b;
       }
       builder f26([[int, int, int], [tuple, tuple], [int, int], [int, int]]
           self, builder b) impure {
         return f27(self, b);
       }
       builder f3(tuple self, builder b) impure {
         {
         tuple temp = self;
       int discr =
       first(temp);
       if (discr == 1)
       {
         [[int, int, int], [tuple, tuple], [int, int], [int, int]] info =
           second(temp);
       {
         builder b = f4(b, 0, 1);
       return
       f26(info, b);
       }} else if (discr == 0)
       {
         [tuple, tuple, int, int] info = second(temp);
       {
         builder b = f4(b, 3, 2);
       return
       f5(info, b);
       }} else
       {
         }}}
       builder f39([] self, builder b) impure {
         return b;
       }
       builder f2([tuple, []] self, builder b) impure {
         builder b = f3(get0(func_believe_me(self)), b);
         builder b = f4(b, 0, 1);
         builder b = f4(b, 0, 1);
         builder b = f39(get1(func_believe_me(self)), b);
         return b;
       }
       _ test([tuple, []] m) impure {
         builder b = f1();
         builder b = f2(m, b);
       } |}]

let%expect_test "deserialization api" =
  let source =
    {|
      struct Empty {
        impl Deserialize {
          fn deserialize(s: Slice) -> LoadResult[Self] {
            return LoadResult[Self].new(s, Self{});
          }
        }
      }
      fn test(c: Cell) {
        let s = Slice.parse(c);
        let msg = Message[Empty].deserialize(s);
      }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       forall A -> A get1(tuple t) asm "1 INDEX";
       forall A -> A get0(tuple t) asm "0 INDEX";
       int builtin_gt(int i1, int i2) impure {
         return _>_(i1, i2);
       }
       int builtin_geq(int i1, int i2) impure {
         return _>=_(i1, i2);
       }
       int builtin_lt(int i1, int i2) impure {
         return _<_(i1, i2);
       }
       int builtin_leq(int i1, int i2) impure {
         return _<=_(i1, i2);
       }
       int builtin_neq(int i1, int i2) impure {
         return _!=_(i1, i2);
       }
       int builtin_eq(int i1, int i2) impure {
         return _==_(i1, i2);
       }
       int builtin_bit_or(int i1, int i2) impure {
         return _|_(i1, i2);
       }
       int builtin_bit_and(int i1, int i2) impure {
         return _&_(i1, i2);
       }
       int builtin_div(int i1, int i2) impure {
         return _/_(i1, i2);
       }
       int builtin_mul(int i1, int i2) impure {
         return _*_(i1, i2);
       }
       int builtin_sub(int i1, int i2) impure {
         return _-_(i1, i2);
       }
       int builtin_add(int i1, int i2) impure {
         return _+_(i1, i2);
       }
       int builtin_not(int c) impure {
         return func_bit_not(c);
       }
       _ believe_me(_ i) impure {
         return func_believe_me(i);
       }
       _ builtin_accept_message() impure {
         return accept_message();
       }
       int builtin_now() impure {
         return now();
       }
       int builtin_slice_hash(slice s) impure {
         return slice_hash(s);
       }
       int builtin_check_signature(int h, slice s, int k) impure {
         return check_signature(h, s, k);
       }
       _ builtin_set_data(cell d) impure {
         return set_data(d);
       }
       cell builtin_get_data() impure {
         return get_data();
       }
       _ builtin_throw(int e) impure {
         return throw(e);
       }
       _ builtin_send_raw_message(cell c, int f) impure {
         return send_raw_message(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) impure {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) impure {
         return end_parse(s);
       }
       int builtin_slice_refs(slice s) impure {
         return slice_refs(s);
       }
       slice builtin_slice_last(slice s, int l) impure {
         return slice_last(s, l);
       }
       (slice, cell) builtin_load_ref(slice s) impure {
         return load_ref(s);
       }
       (slice, int) builtin_load_grams(slice s) impure {
         return load_grams(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) impure {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) impure {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) impure {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) impure {
         return begin_parse(c);
       }
       builder builtin_store_grams(builder b, int c) impure {
         return store_grams(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) impure {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) impure {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) impure {
         return end_cell(b);
       }
       builder builtin_begin_cell() impure {
         return begin_cell();
       }
       _ thrown(int n) impure {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) impure {
         return builtin_send_raw_message(msg, flags);
       }
       int f0(int i) impure {
         return i;
       }
       int hash_of_slice(slice s) impure {
         return f0(builtin_slice_hash(s));
       }
       int is_signature_valid(int hash, slice sign, int pubkey) impure {
         return builtin_check_signature(hash, sign, pubkey);
       }
       slice f1(cell cell_) impure {
         return builtin_begin_parse(cell_);
       }
       [slice, int] f5(slice self, int bits) impure {
         (slice, int) output = builtin_load_uint(self, bits);
         slice slice_ = tensor2_value1(output);
         int int_ = tensor2_value2(output);
         return [believe_me(slice_), int_];
       }
       [slice, int] f12(slice self, int bits) impure {
         (slice, int) output = builtin_load_int(self, bits);
         slice slice_ = tensor2_value1(output);
         int int_ = tensor2_value2(output);
         return [believe_me(slice_), int_];
       }
       [slice, int] f11(slice s) impure {
         [slice, int] res = f12(s, 9);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int]] f10(slice slice_) impure {
         [slice slice_, int len] = f11(slice_);
         [slice slice_, int bits] = f12(slice_, len);
         return [slice_, [len, bits]];
       }
       [slice, tuple] f13(tuple v, slice s) impure {
         return [s, v];
       }
       [slice, []] f15([] v, slice s) impure {
         return [s, v];
       }
       [slice, []] f14(slice s) impure {
         return f15(s, []);
       }
       [slice, tuple] f9(slice slice_) impure {
         [slice, int] res_discr = f5(slice_, 1);
         if (builtin_eq(get1(func_believe_me(res_discr)), 0)) {
         [slice, []] res = f14(get0(func_believe_me(res_discr)));
       return
       f13(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       {
         [slice, int] res_discr = f5(get0(func_believe_me(res_discr)), 1);
       if (builtin_eq(get1(func_believe_me(res_discr)), 1))
       {
         [slice, [int, int]] res = f10(get0(func_believe_me(res_discr)));
       return
       f13(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       throw(0);
       }}
       [slice, tuple] f8(slice s) impure {
         return f9(s);
       }
       [slice, int] f19(slice s) impure {
         [slice, int] res = f12(s, 32);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int, int]] f20([int, int, int] v, slice s) impure {
         return [s, v];
       }
       [slice, [int, int, int]] f18(slice s) impure {
         [slice slice_, int anycast] = f12(s, 1);
         if (builtin_eq(anycast, 0)) {
         [slice slice_, int len] = f11(slice_);
       [slice slice_, int workchain_id] =
       f19(slice_);
       [slice slice_, int address] =
       f12(slice_, len);
       return
       f20(slice_, [len, workchain_id, address]);
       } else
       {
         thrown(0);
       }}
       [slice, tuple] f21(tuple v, slice s) impure {
         return [s, v];
       }
       [slice, int] f24(slice s) impure {
         [slice, int] res = f12(s, 8);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, int] f25(slice s) impure {
         [slice, int] res = f12(s, 256);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int]] f23(slice slice_) impure {
         [slice slice_, int workchain_id] = f24(slice_);
         [slice slice_, int address] = f25(slice_);
         return [slice_, [workchain_id, address]];
       }
       [slice, [int, int]] f22(slice s) impure {
         [slice, int] res_anycast = f12(s, 1);
         if (builtin_eq(get1(func_believe_me(res_anycast)), 0)) {
         return f23(s);
       } else
       {
         thrown(0);
       }}
       [slice, tuple] f17(slice slice_) impure {
         [slice, int] res_discr = f5(slice_, 1);
         if (builtin_eq(get1(func_believe_me(res_discr)), 0)) {
         [slice, [int, int]] res = f22(get0(func_believe_me(res_discr)));
       return
       f21(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       {
         [slice, int] res_discr = f5(get0(func_believe_me(res_discr)), 1);
       if (builtin_eq(get1(func_believe_me(res_discr)), 1))
       {
         [slice, [int, int, int]] res = f18(get0(func_believe_me(res_discr)));
       return
       f21(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       throw(0);
       }}
       [slice, tuple] f16(slice s) impure {
         return f17(s);
       }
       [slice, int] f27(slice self) impure {
         (slice, int) output = builtin_load_grams(self);
         slice slice_ = tensor2_value1(output);
         int coins = tensor2_value2(output);
         return [believe_me(slice_), coins];
       }
       [slice, int] f28(int v, slice s) impure {
         return [s, v];
       }
       [slice, int] f26(slice s) impure {
         [slice slice_, int value] = f27(s);
         return f28(value, slice_);
       }
       [slice, [tuple, tuple, int]] f7(slice slice_) impure {
         [slice slice_, tuple src] = f8(slice_);
         [slice slice_, tuple dest] = f16(slice_);
         [slice slice_, int import_fee] = f26(slice_);
         return [slice_, [src, dest, import_fee]];
       }
       [slice, [tuple, tuple, int]] f6(slice s) impure {
         return f7(s);
       }
       [slice, tuple] f29(tuple v, slice s) impure {
         return [s, v];
       }
       [slice, int] f34(slice s) impure {
         [slice, int] res = f12(s, 1);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int, int]] f33(slice slice_) impure {
         [slice slice_, int ihr_disabled] = f34(slice_);
         [slice slice_, int bounce] = f34(slice_);
         [slice slice_, int bounced] = f34(slice_);
         return [slice_, [ihr_disabled, bounce, bounced]];
       }
       [slice, [int, int, int]] f32(slice s) impure {
         return f33(s);
       }
       [slice, [tuple, tuple]] f36(slice slice_) impure {
         [slice slice_, tuple src] = f16(slice_);
         [slice slice_, tuple dst] = f16(slice_);
         return [slice_, [src, dst]];
       }
       [slice, [tuple, tuple]] f35(slice s) impure {
         return f36(s);
       }
       [slice, [int, int]] f38(slice slice_) impure {
         [slice slice_, int ihr_fee] = f26(slice_);
         [slice slice_, int fwd_fee] = f26(slice_);
         return [slice_, [ihr_fee, fwd_fee]];
       }
       [slice, [int, int]] f37(slice s) impure {
         return f38(s);
       }
       [slice, int] f41(slice s) impure {
         [slice, int] res = f5(s, 64);
         return [get0(func_believe_me(res)), get1(func_believe_me(res))];
       }
       [slice, int] f42(slice s) impure {
         [slice, int] res = f5(s, 32);
         return [get0(func_believe_me(res)), get1(func_believe_me(res))];
       }
       [slice, [int, int]] f40(slice slice_) impure {
         [slice slice_, int created_lt] = f41(slice_);
         [slice slice_, int created_at] = f42(slice_);
         return [slice_, [created_lt, created_at]];
       }
       [slice, [int, int]] f39(slice s) impure {
         return f40(s);
       }
       [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] f31(slice
           slice_) impure {
         [slice slice_, [int, int, int] flags] = f32(slice_);
         [slice slice_, [tuple, tuple] addresses] = f35(slice_);
         [slice slice_, [int, int] coins] = f37(slice_);
         [slice slice_, [int, int] timestamps] = f39(slice_);
         return [slice_, [flags, addresses, coins, timestamps]];
       }
       [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] f30(slice s)
           impure {
         return f31(s);
       }
       [slice, tuple] f4(slice slice_) impure {
         [slice, int] res_discr = f5(slice_, 1);
         if (builtin_eq(get1(func_believe_me(res_discr)), 0)) {
         [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] res =
           f30(get0(func_believe_me(res_discr)));
       return
       f29(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       {
         [slice, int] res_discr = f5(get0(func_believe_me(res_discr)), 1);
       if (builtin_eq(get1(func_believe_me(res_discr)), 1))
       {
         [slice, [tuple, tuple, int]] res = f6(get0(func_believe_me(res_discr)));
       return
       f29(get0(func_believe_me(res)), get1(func_believe_me(res)));
       } else
       throw(0);
       }}
       [slice, tuple] f3(slice s) impure {
         return f4(s);
       }
       [slice, []] f44([] v, slice s) impure {
         return [s, v];
       }
       [slice, []] f43(slice s) impure {
         return f44(s, []);
       }
       [slice, [tuple, []]] f45([tuple, []] v, slice s) impure {
         return [s, v];
       }
       [slice, [tuple, []]] f2(slice s) impure {
         [slice slice_, tuple info] = f3(s);
         [slice slice_, int init] = f12(slice_, 1);
         if (builtin_eq(init, 0)) {
         [slice slice_, int discr] = f12(slice_, 1);
       if (builtin_eq(discr, 0))
       {
         [slice slice_, [] body] = f43(slice_);
       [tuple, _] mes =
       [info, believe_me(body)];
       return
       f45(mes, slice_);
       } else
       {
         }} else
       {
         thrown(0);
       }}
       _ test(cell c) impure {
         slice s = f1(c);
         [slice, [tuple, []]] msg = f2(s);
       } |}]

let%expect_test "destructuring let" =
  let source =
    {|
      struct T {
        val x: Integer
        val y: Integer
        val z: Integer
      }
      fn test(t: T) -> Integer {
        let {x, y as y2, z} = t;
        y2
      }
     |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int test([int, int, int] t) impure {
         [int x, int y2, int z] = t;
         return y2;
       }
    |}]

let%expect_test "destructuring let with rest ignored" =
  let source =
    {|
      struct T {
        val x: Integer
        val y: Integer
        val z: Integer
      }
      fn test(t: T) -> Integer {
        let {y as y2, ..} = t;
        y2
      }
     |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
       forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
         (Value1 value, _) = tensor;
         return value;
       }
       forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
         (_, Value2 value) = tensor;
         return value;
       }
       forall A, B -> B func_believe_me(A i) asm "NOP";
       int func_bit_not(int a) {
         return ~ a;
       }
       int test([int, int, int] t) impure {
         [_, int y2, _] = t;
         return y2;
       }
    |}]

let%expect_test "deserializer" =
  let source =
    {|
      struct Something {
        val value1: Int[9]
        val value2: Int[256]
      }
      let test = deserializer[Something];
    |}
  in
  pp_codegen source ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    int builtin_gt(int i1, int i2) impure {
      return _>_(i1, i2);
    }
    int builtin_geq(int i1, int i2) impure {
      return _>=_(i1, i2);
    }
    int builtin_lt(int i1, int i2) impure {
      return _<_(i1, i2);
    }
    int builtin_leq(int i1, int i2) impure {
      return _<=_(i1, i2);
    }
    int builtin_neq(int i1, int i2) impure {
      return _!=_(i1, i2);
    }
    int builtin_eq(int i1, int i2) impure {
      return _==_(i1, i2);
    }
    int builtin_bit_or(int i1, int i2) impure {
      return _|_(i1, i2);
    }
    int builtin_bit_and(int i1, int i2) impure {
      return _&_(i1, i2);
    }
    int builtin_div(int i1, int i2) impure {
      return _/_(i1, i2);
    }
    int builtin_mul(int i1, int i2) impure {
      return _*_(i1, i2);
    }
    int builtin_sub(int i1, int i2) impure {
      return _-_(i1, i2);
    }
    int builtin_add(int i1, int i2) impure {
      return _+_(i1, i2);
    }
    int builtin_not(int c) impure {
      return func_bit_not(c);
    }
    _ believe_me(_ i) impure {
      return func_believe_me(i);
    }
    _ builtin_accept_message() impure {
      return accept_message();
    }
    int builtin_now() impure {
      return now();
    }
    int builtin_slice_hash(slice s) impure {
      return slice_hash(s);
    }
    int builtin_check_signature(int h, slice s, int k) impure {
      return check_signature(h, s, k);
    }
    _ builtin_set_data(cell d) impure {
      return set_data(d);
    }
    cell builtin_get_data() impure {
      return get_data();
    }
    _ builtin_throw(int e) impure {
      return throw(e);
    }
    _ builtin_send_raw_message(cell c, int f) impure {
      return send_raw_message(c, f);
    }
    (int, int) builtin_divmod(int i1, int i2) impure {
      return divmod(i1, i2);
    }
    _ builtin_end_parse(slice s) impure {
      return end_parse(s);
    }
    int builtin_slice_refs(slice s) impure {
      return slice_refs(s);
    }
    slice builtin_slice_last(slice s, int l) impure {
      return slice_last(s, l);
    }
    (slice, cell) builtin_load_ref(slice s) impure {
      return load_ref(s);
    }
    (slice, int) builtin_load_grams(slice s) impure {
      return load_grams(s);
    }
    (slice, slice) builtin_load_bits(slice s, int bs) impure {
      return load_bits(s, bs);
    }
    (slice, int) builtin_load_uint(slice s, int bs) impure {
      return load_uint(s, bs);
    }
    (slice, int) builtin_load_int(slice s, int bs) impure {
      return load_int(s, bs);
    }
    slice builtin_begin_parse(cell c) impure {
      return begin_parse(c);
    }
    builder builtin_store_grams(builder b, int c) impure {
      return store_grams(b, c);
    }
    builder builtin_store_uint(builder b, int i, int bs) impure {
      return store_uint(b, i, bs);
    }
    builder builtin_store_int(builder b, int i, int bs) impure {
      return store_int(b, i, bs);
    }
    cell builtin_end_cell(builder b) impure {
      return end_cell(b);
    }
    builder builtin_begin_cell() impure {
      return begin_cell();
    }
    _ thrown(int n) impure {
      return builtin_throw(n);
    }
    _ send_raw_msg(cell msg, int flags) impure {
      return builtin_send_raw_message(msg, flags);
    }
    int f0(int i) impure {
      return i;
    }
    int hash_of_slice(slice s) impure {
      return f0(builtin_slice_hash(s));
    }
    int is_signature_valid(int hash, slice sign, int pubkey) impure {
      return builtin_check_signature(hash, sign, pubkey);
    }
    [slice, int] f2(slice self, int bits) impure {
      (slice, int) output = builtin_load_int(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [believe_me(slice_), int_];
    }
    [slice, int] f1(slice s) impure {
      [slice, int] res = f2(s, 9);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, int] f3(slice s) impure {
      [slice, int] res = f2(s, 256);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, [int, int]] test(slice slice_) impure {
      [slice slice_, int value1] = f1(slice_);
      [slice slice_, int value2] = f3(slice_);
      return [slice_, [value1, value2]];
    } |}]

let%expect_test "deserializer unions" =
  let source =
    {|
      union TestUnion {
        case Int[8]
        case Int[9]
      }
      let deserialize_union = deserializer[TestUnion];
    |}
  in
  pp_codegen source ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    forall A -> A get1(tuple t) asm "1 INDEX";
    forall A -> A get0(tuple t) asm "0 INDEX";
    int builtin_gt(int i1, int i2) impure {
      return _>_(i1, i2);
    }
    int builtin_geq(int i1, int i2) impure {
      return _>=_(i1, i2);
    }
    int builtin_lt(int i1, int i2) impure {
      return _<_(i1, i2);
    }
    int builtin_leq(int i1, int i2) impure {
      return _<=_(i1, i2);
    }
    int builtin_neq(int i1, int i2) impure {
      return _!=_(i1, i2);
    }
    int builtin_eq(int i1, int i2) impure {
      return _==_(i1, i2);
    }
    int builtin_bit_or(int i1, int i2) impure {
      return _|_(i1, i2);
    }
    int builtin_bit_and(int i1, int i2) impure {
      return _&_(i1, i2);
    }
    int builtin_div(int i1, int i2) impure {
      return _/_(i1, i2);
    }
    int builtin_mul(int i1, int i2) impure {
      return _*_(i1, i2);
    }
    int builtin_sub(int i1, int i2) impure {
      return _-_(i1, i2);
    }
    int builtin_add(int i1, int i2) impure {
      return _+_(i1, i2);
    }
    int builtin_not(int c) impure {
      return func_bit_not(c);
    }
    _ believe_me(_ i) impure {
      return func_believe_me(i);
    }
    _ builtin_accept_message() impure {
      return accept_message();
    }
    int builtin_now() impure {
      return now();
    }
    int builtin_slice_hash(slice s) impure {
      return slice_hash(s);
    }
    int builtin_check_signature(int h, slice s, int k) impure {
      return check_signature(h, s, k);
    }
    _ builtin_set_data(cell d) impure {
      return set_data(d);
    }
    cell builtin_get_data() impure {
      return get_data();
    }
    _ builtin_throw(int e) impure {
      return throw(e);
    }
    _ builtin_send_raw_message(cell c, int f) impure {
      return send_raw_message(c, f);
    }
    (int, int) builtin_divmod(int i1, int i2) impure {
      return divmod(i1, i2);
    }
    _ builtin_end_parse(slice s) impure {
      return end_parse(s);
    }
    int builtin_slice_refs(slice s) impure {
      return slice_refs(s);
    }
    slice builtin_slice_last(slice s, int l) impure {
      return slice_last(s, l);
    }
    (slice, cell) builtin_load_ref(slice s) impure {
      return load_ref(s);
    }
    (slice, int) builtin_load_grams(slice s) impure {
      return load_grams(s);
    }
    (slice, slice) builtin_load_bits(slice s, int bs) impure {
      return load_bits(s, bs);
    }
    (slice, int) builtin_load_uint(slice s, int bs) impure {
      return load_uint(s, bs);
    }
    (slice, int) builtin_load_int(slice s, int bs) impure {
      return load_int(s, bs);
    }
    slice builtin_begin_parse(cell c) impure {
      return begin_parse(c);
    }
    builder builtin_store_grams(builder b, int c) impure {
      return store_grams(b, c);
    }
    builder builtin_store_uint(builder b, int i, int bs) impure {
      return store_uint(b, i, bs);
    }
    builder builtin_store_int(builder b, int i, int bs) impure {
      return store_int(b, i, bs);
    }
    cell builtin_end_cell(builder b) impure {
      return end_cell(b);
    }
    builder builtin_begin_cell() impure {
      return begin_cell();
    }
    _ thrown(int n) impure {
      return builtin_throw(n);
    }
    _ send_raw_msg(cell msg, int flags) impure {
      return builtin_send_raw_message(msg, flags);
    }
    int f0(int i) impure {
      return i;
    }
    int hash_of_slice(slice s) impure {
      return f0(builtin_slice_hash(s));
    }
    int is_signature_valid(int hash, slice sign, int pubkey) impure {
      return builtin_check_signature(hash, sign, pubkey);
    }
    [slice, int] f1(slice self, int bits) impure {
      (slice, int) output = builtin_load_uint(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [believe_me(slice_), int_];
    }
    [slice, int] f3(slice self, int bits) impure {
      (slice, int) output = builtin_load_int(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [believe_me(slice_), int_];
    }
    [slice, int] f2(slice s) impure {
      [slice, int] res = f3(s, 9);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, tuple] f4(tuple v, slice s) impure {
      return [s, v];
    }
    [slice, int] f5(slice s) impure {
      [slice, int] res = f3(s, 8);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, tuple] deserialize_union(slice slice_) impure {
      [slice, int] res_discr = f1(slice_, 1);
      if (builtin_eq(get1(func_believe_me(res_discr)), 0)) {
      [slice, int] res = f5(get0(func_believe_me(res_discr)));
    return
    f4(get0(func_believe_me(res)), get1(func_believe_me(res)));
    } else
    {
      [slice, int] res_discr = f1(get0(func_believe_me(res_discr)), 1);
    if (builtin_eq(get1(func_believe_me(res_discr)), 1))
    {
      [slice, int] res = f2(get0(func_believe_me(res_discr)));
    return
    f4(get0(func_believe_me(res)), get1(func_believe_me(res)));
    } else
    throw(0);
    }} |}]

let%expect_test "assignment" =
  let source =
    {|
      fn test(x: Integer) {
        let a = 1;
        a = x;
        a
      }
  |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    int test(int x) impure {
      int a = 1;
      a = x;
      return a;
    } |}]

let%expect_test "assignment with condition block" =
  let source =
    {|
      fn test(x: Int[257]) {
        let a = 1;
        if (true) {
          a = 10;
        } else {
          a = 20;
        }
        return a;
      }
    |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    int test(int x) impure {
      int a = 1;
      if (-1) {
      a = 10;
    } else
    {
      a = 20;
    }  return a;
    } |}]

let%expect_test "codegen while block" =
  let source =
    {|
      fn test() {
        let a = 10;
        while (true) {
          a = 20;
        }
      }
    |}
  in
  pp_codegen source ~strip_defaults:true ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    _ test() impure {
      int a = 10;
      while (-1) {
      a = 20;
    }} |}]

let%expect_test "codegen while block" =
  let source =
    {|
    // TODO: remove this type.
    struct MsgBodyData {
      val subwallet: Uint[32]
      val valid_until: Uint[32]
      val seqno: Uint[32]

      @derive impl Deserialize {}
    }
    struct MsgBody {
      val signature: Signature
      val data: MsgBodyData
      val rest: RestSlice
    
      @derive impl Deserialize {}
    }
    
    struct WalletState {
      val seqno: Uint[32]
      val subwallet: Uint[32]
      val public_key: Uint[256]
    
      @derive impl Deserialize {}
      @derive impl Serialize {}
    }
    
    struct NextMessage {
      val cell: RefCell
      val flags: SendRawMsgFlags
      
      @derive impl Deserialize {}
    }
    
    fn recv_internal(_: Slice) {}
    
    fn recv_external(input: Slice) {
      let body = MsgBody.deserialize(input).value;
      let data = body.data;
      let state = WalletState.deserialize(Slice.parse(builtin_get_data())).value;
      if (builtin_leq(data.valid_until.value, builtin_now())) { thrown(35) }
      if (builtin_neq(data.seqno.value, state.seqno.value)) { thrown(33) }
      if (builtin_neq(data.subwallet.value, state.subwallet.value)) { thrown(34) }
      if (builtin_not(body.signature.is_valid(state.public_key))) { thrown(35) }
    
      builtin_accept_message();
    
      let slice = body.rest.inner;
      while (builtin_neq(slice.refs_count(), 0)) {
        let {value as next, slice as new_slice} = NextMessage.deserialize(slice);
        slice = new_slice;
        send_raw_msg(next.cell.inner, next.flags);
      }
    
      let new_state = WalletState {
        seqno: builtin_add(state.seqno.value, 1),
        subwallet: state.subwallet,
        public_key: state.public_key
      };
      let new_state = new_state.serialize(Builder.new()).build();
      builtin_set_data(new_state.c);
    }
    |}
  in
  pp_codegen source ;
  [%expect
    {|
    forall Value1, Value2 -> Value1 tensor2_value1((Value1, Value2) tensor) {
      (Value1 value, _) = tensor;
      return value;
    }
    forall Value1, Value2 -> Value2 tensor2_value2((Value1, Value2) tensor) {
      (_, Value2 value) = tensor;
      return value;
    }
    forall A, B -> B func_believe_me(A i) asm "NOP";
    int func_bit_not(int a) {
      return ~ a;
    }
    forall A -> A get2(tuple t) asm "2 INDEX";
    forall A -> A get1(tuple t) asm "1 INDEX";
    forall A -> A get0(tuple t) asm "0 INDEX";
    int builtin_gt(int i1, int i2) impure {
      return _>_(i1, i2);
    }
    int builtin_geq(int i1, int i2) impure {
      return _>=_(i1, i2);
    }
    int builtin_lt(int i1, int i2) impure {
      return _<_(i1, i2);
    }
    int builtin_leq(int i1, int i2) impure {
      return _<=_(i1, i2);
    }
    int builtin_neq(int i1, int i2) impure {
      return _!=_(i1, i2);
    }
    int builtin_eq(int i1, int i2) impure {
      return _==_(i1, i2);
    }
    int builtin_bit_or(int i1, int i2) impure {
      return _|_(i1, i2);
    }
    int builtin_bit_and(int i1, int i2) impure {
      return _&_(i1, i2);
    }
    int builtin_div(int i1, int i2) impure {
      return _/_(i1, i2);
    }
    int builtin_mul(int i1, int i2) impure {
      return _*_(i1, i2);
    }
    int builtin_sub(int i1, int i2) impure {
      return _-_(i1, i2);
    }
    int builtin_add(int i1, int i2) impure {
      return _+_(i1, i2);
    }
    int builtin_not(int c) impure {
      return func_bit_not(c);
    }
    _ believe_me(_ i) impure {
      return func_believe_me(i);
    }
    _ builtin_accept_message() impure {
      return accept_message();
    }
    int builtin_now() impure {
      return now();
    }
    int builtin_slice_hash(slice s) impure {
      return slice_hash(s);
    }
    int builtin_check_signature(int h, slice s, int k) impure {
      return check_signature(h, s, k);
    }
    _ builtin_set_data(cell d) impure {
      return set_data(d);
    }
    cell builtin_get_data() impure {
      return get_data();
    }
    _ builtin_throw(int e) impure {
      return throw(e);
    }
    _ builtin_send_raw_message(cell c, int f) impure {
      return send_raw_message(c, f);
    }
    (int, int) builtin_divmod(int i1, int i2) impure {
      return divmod(i1, i2);
    }
    _ builtin_end_parse(slice s) impure {
      return end_parse(s);
    }
    int builtin_slice_refs(slice s) impure {
      return slice_refs(s);
    }
    slice builtin_slice_last(slice s, int l) impure {
      return slice_last(s, l);
    }
    (slice, cell) builtin_load_ref(slice s) impure {
      return load_ref(s);
    }
    (slice, int) builtin_load_grams(slice s) impure {
      return load_grams(s);
    }
    (slice, slice) builtin_load_bits(slice s, int bs) impure {
      return load_bits(s, bs);
    }
    (slice, int) builtin_load_uint(slice s, int bs) impure {
      return load_uint(s, bs);
    }
    (slice, int) builtin_load_int(slice s, int bs) impure {
      return load_int(s, bs);
    }
    slice builtin_begin_parse(cell c) impure {
      return begin_parse(c);
    }
    builder builtin_store_grams(builder b, int c) impure {
      return store_grams(b, c);
    }
    builder builtin_store_uint(builder b, int i, int bs) impure {
      return store_uint(b, i, bs);
    }
    builder builtin_store_int(builder b, int i, int bs) impure {
      return store_int(b, i, bs);
    }
    cell builtin_end_cell(builder b) impure {
      return end_cell(b);
    }
    builder builtin_begin_cell() impure {
      return begin_cell();
    }
    _ thrown(int n) impure {
      return builtin_throw(n);
    }
    _ send_raw_msg(cell msg, int flags) impure {
      return builtin_send_raw_message(msg, flags);
    }
    int f0(int i) impure {
      return i;
    }
    int hash_of_slice(slice s) impure {
      return f0(builtin_slice_hash(s));
    }
    int is_signature_valid(int hash, slice sign, int pubkey) impure {
      return builtin_check_signature(hash, sign, pubkey);
    }
    _ recv_internal(slice _) impure {
    }
    [slice, slice] f5(slice self, int bits) impure {
      (slice, slice) output = builtin_load_bits(self, bits);
      slice slice_ = tensor2_value1(output);
      slice slice2 = tensor2_value2(output);
      return [believe_me(slice_), believe_me(slice2)];
    }
    [slice, slice] f6(slice v, slice s) impure {
      return [s, v];
    }
    [slice, slice] f4(slice s) impure {
      [slice slice_, slice value] = f5(s, 512);
      return f6(value, slice_);
    }
    [slice, [slice, slice]] f7([slice, slice] v, slice s) impure {
      return [s, v];
    }
    [slice, [slice, slice]] f3(slice s) impure {
      [slice slice_, slice sig] = f4(s);
      return f7([sig, slice_], slice_);
    }
    [slice, int] f11(slice self, int bits) impure {
      (slice, int) output = builtin_load_uint(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [believe_me(slice_), int_];
    }
    [slice, int] f10(slice s) impure {
      [slice, int] res = f11(s, 32);
      return [get0(func_believe_me(res)), get1(func_believe_me(res))];
    }
    [slice, [int, int, int]] f9(slice slice_) impure {
      [slice slice_, int subwallet] = f10(slice_);
      [slice slice_, int valid_until] = f10(slice_);
      [slice slice_, int seqno] = f10(slice_);
      return [slice_, [subwallet, valid_until, seqno]];
    }
    [slice, [int, int, int]] f8(slice s) impure {
      return f9(s);
    }
    [slice, slice] f13(slice v, slice s) impure {
      return [s, v];
    }
    [slice, slice] f12(slice s) impure {
      slice empty_slice = builtin_slice_last(s, 0);
      return f13(s, empty_slice);
    }
    [slice, [[slice, slice], [int, int, int], slice]] f2(slice slice_) impure {
      [slice slice_, [slice, slice] signature] = f3(slice_);
      [slice slice_, [int, int, int] data] = f8(slice_);
      [slice slice_, slice rest] = f12(slice_);
      return [slice_, [signature, data, rest]];
    }
    [slice, [[slice, slice], [int, int, int], slice]] f1(slice s) impure {
      return f2(s);
    }
    slice f14(cell cell_) impure {
      return builtin_begin_parse(cell_);
    }
    [slice, int] f17(slice s) impure {
      [slice, int] res = f11(s, 256);
      return [get0(func_believe_me(res)), get1(func_believe_me(res))];
    }
    [slice, [int, int, int]] f16(slice slice_) impure {
      [slice slice_, int seqno] = f10(slice_);
      [slice slice_, int subwallet] = f10(slice_);
      [slice slice_, int public_key] = f17(slice_);
      return [slice_, [seqno, subwallet, public_key]];
    }
    [slice, [int, int, int]] f15(slice s) impure {
      return f16(s);
    }
    int f18([slice, slice] self, int public_key) impure {
      return
        is_signature_valid(hash_of_slice(get1(func_believe_me(self))), get0(func_believe_me(self)), public_key);
    }
    [slice, cell] f22(slice self) impure {
      (slice, cell) output = builtin_load_ref(self);
      slice slice_ = tensor2_value1(output);
      cell ref = tensor2_value2(output);
      return [believe_me(slice_), ref];
    }
    [slice, cell] f23(cell v, slice s) impure {
      return [s, v];
    }
    [slice, cell] f21(slice s) impure {
      [slice slice_, cell value] = f22(s);
      return f23(value, slice_);
    }
    [slice, int] f27(slice self, int bits) impure {
      (slice, int) output = builtin_load_int(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [believe_me(slice_), int_];
    }
    [slice, int] f26(slice s) impure {
      [slice, int] res = f27(s, 8);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, int] f25(slice slice_) impure {
      [slice slice_, int value] = f26(slice_);
      return [slice_, value];
    }
    [slice, int] f24(slice s) impure {
      return f25(s);
    }
    [slice, [cell, int]] f20(slice slice_) impure {
      [slice slice_, cell cell_] = f21(slice_);
      [slice slice_, int flags] = f24(slice_);
      return [slice_, [cell_, flags]];
    }
    [slice, [cell, int]] f19(slice s) impure {
      return f20(s);
    }
    int f28(slice self) impure {
      return builtin_slice_refs(self);
    }
    int f29(int i) impure {
      return i;
    }
    builder f30() impure {
      return builtin_begin_cell();
    }
    builder f34(builder self, int uint, int bits) impure {
      return builtin_store_uint(self, uint, bits);
    }
    builder f33(int self, builder builder_) impure {
      return f34(builder_, self, 32);
    }
    builder f35(int self, builder builder_) impure {
      return f34(builder_, self, 256);
    }
    builder f32([int, int, int] self, builder b) impure {
      builder b = f33(get0(func_believe_me(self)), b);
      builder b = f33(get1(func_believe_me(self)), b);
      builder b = f35(get2(func_believe_me(self)), b);
      return b;
    }
    builder f31([int, int, int] self, builder b) impure {
      return f32(self, b);
    }
    cell f36(builder self) impure {
      return builtin_end_cell(self);
    }
    _ recv_external(slice input) impure {
      [[slice, slice], [int, int, int], slice] body =
        get1(func_believe_me(f1(input)));
      [int, int, int] data = get1(func_believe_me(body));
      [int, int, int] state = get1(func_believe_me(f15(f14(builtin_get_data()))));
      if (builtin_leq(get1(func_believe_me(data)), builtin_now())) {
      thrown(35);
    }  if (builtin_neq(get2(func_believe_me(data)), get0(func_believe_me(state))))
         {
      thrown(33);
    }  if (builtin_neq(get0(func_believe_me(data)), get1(func_believe_me(state))))
         {
      thrown(34);
    }  if
         (builtin_not(f18(get0(func_believe_me(body)), get2(func_believe_me(state)))))
         {
      thrown(35);
    }  builtin_accept_message();
      slice slice_ = get2(func_believe_me(body));
      while (builtin_neq(f28(slice_), 0)) {
      [slice new_slice, [cell, int] next] = f19(slice_);
    slice_ =
    new_slice;
    send_raw_msg(get0(func_believe_me(next)), get1(func_believe_me(next)));
    }  [int, int, int] new_state =
         [f29(builtin_add(get0(func_believe_me(state)), 1)), get1(func_believe_me(state)), get2(func_believe_me(state))];
      cell new_state = f36(f31(new_state, f30()));
      return builtin_set_data(new_state);
    } |}]
