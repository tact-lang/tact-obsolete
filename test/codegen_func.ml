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
    int test() {
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
       int test([int, int] t) {
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
       int test(int value) {
         return value;
       }
       int test2(int value) {
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       builder f1(builder self, int int_, int bits) {
         return builtin_store_int(self, int_, bits);
       }
       builder f0(int self, builder builder_) {
         return f1(builder_, self, 32);
       }
       _ test_int(builder b) {
         int i = 100;
         return f0(100, b);
       } |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
      struct T {
        val a: Int[32]
        val b: Int[16]
      }
      let T_serializer = serializer(T);

      fn test() {
        let b = Builder.new();
        T_serializer(T{a: Int(32).new(0), b: Int(16).new(1)}, b);
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       builder f1(builder self, int int_, int bits) {
         return builtin_store_int(self, int_, bits);
       }
       builder f0(int self, builder builder_) {
         return f1(builder_, self, 32);
       }
       builder f2(int self, builder builder_) {
         return f1(builder_, self, 16);
       }
       builder T_serializer([int, int] self, builder b) {
         builder b = f0(first(self), b);
         builder b = f2(second(self), b);
         return b;
       }
       builder f3() {
         return builtin_begin_cell();
       }
       _ test() {
         builder b = f3();
         return T_serializer([0, 1], b);
       } |}]

let%expect_test "demo struct serializer 2" =
  let source =
    {|
      struct Foo {
        val a: Int[32]
        val b: Int[16]
      }
      let serialize_foo = serializer(Foo);

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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       builder f1(builder self, int int_, int bits) {
         return builtin_store_int(self, int_, bits);
       }
       builder f0(int self, builder builder_) {
         return f1(builder_, self, 32);
       }
       builder f2(int self, builder builder_) {
         return f1(builder_, self, 16);
       }
       builder serialize_foo([int, int] self, builder b) {
         builder b = f0(first(self), b);
         builder b = f2(second(self), b);
         return b;
       }
       builder f3() {
         return builtin_begin_cell();
       }
       builder test() {
         builder b = f3();
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
         int test(int flag) {
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
         int test(int flag) {
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       builder f1(builder self, int int_, int bits) {
         return builtin_store_int(self, int_, bits);
       }
       builder f0(int self, builder builder_) {
         return f1(builder_, self, 32);
       }
       builder serialize_wallet([int, int] self, builder b) {
         builder b = f0(first(self), b);
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
       tuple try(tuple x) {
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       int test(tuple i) {
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       int test() {
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       builder f0() {
         return builtin_begin_cell();
       }
       builder f3(builder self, int int_, int bits) {
         return builtin_store_int(self, int_, bits);
       }
       builder f10([] self, builder b) {
         return b;
       }
       builder f12(int self, builder builder_) {
         return f3(builder_, self, 9);
       }
       builder f11([int, int] self, builder b) {
         builder b = f12(first(self), b);
         builder b = f3(b, second(self), first(self));
         return b;
       }
       builder f9(tuple self, builder b) {
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
       f11(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         [] var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f10(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f8(tuple self, builder b) {
         return f9(self, b);
       }
       builder f17(int self, builder builder_) {
         return f3(builder_, self, 8);
       }
       builder f18(int self, builder builder_) {
         return f3(builder_, self, 256);
       }
       builder f16([int, int] self, builder b) {
         builder b = f17(first(self), b);
         builder b = f18(second(self), b);
         return b;
       }
       builder f15([int, int] self, builder b) {
         builder b = f3(b, 0, 0);
         return f16(self, b);
       }
       builder f20([int, int, int] self, builder b) {
         builder b = f12(first(self), b);
         builder b = f17(second(self), b);
         return b;
       }
       builder f19([int, int, int] self, builder b) {
         builder b = f3(b, 0, 0);
         builder b = f20(self, b);
         return b;
       }
       builder f14(tuple self, builder b) {
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
       f19(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         [int, int] var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f15(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f13(tuple self, builder b) {
         return f14(self, b);
       }
       builder f7(tuple self, builder b) {
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
       f13(var, b);
       return
       b;
       }} else if (discr == 0)
       {
         tuple var = second(temp);
       {
         _ b = store_uint(b, 0, 1);
       builder b =
       f8(var, b);
       return
       b;
       }} else
       {
         }}}
       builder f6(tuple self, builder b) {
         return f7(self, b);
       }
       builder f22(builder self, int uint, int bits) {
         return builtin_store_uint(self, uint, bits);
       }
       builder f21(int self, builder builder_) {
         return f22(builder_, self, 64);
       }
       builder f23(int self, builder builder_) {
         return f22(builder_, self, 32);
       }
       builder f5([tuple, tuple, int, int] self, builder b) {
         builder b = f6(first(self), b);
         builder b = f8(second(self), b);
         builder b = f21(third(self), b);
         builder b = f23(fourth(self), b);
         return b;
       }
       builder f4([tuple, tuple, int, int] self, builder b) {
         return f5(self, b);
       }
       builder f28(int self, builder builder_) {
         return f3(builder_, self, 1);
       }
       builder f27([int, int, int] self, builder b) {
         builder b = f28(first(self), b);
         builder b = f28(second(self), b);
         builder b = f28(third(self), b);
         return b;
       }
       builder f26([int, int, int] self, builder b) {
         return f27(self, b);
       }
       builder f30([tuple, tuple] self, builder b) {
         builder b = f6(first(self), b);
         builder b = f13(second(self), b);
         return b;
       }
       builder f29([tuple, tuple] self, builder b) {
         return f30(self, b);
       }
       builder f34(builder self, int c) {
         return builtin_store_coins(self, c);
       }
       builder f33(int self, builder builder_) {
         return f34(builder_, self);
       }
       builder f32([int, int] self, builder b) {
         builder b = f33(first(self), b);
         builder b = f33(second(self), b);
         return b;
       }
       builder f31([int, int] self, builder b) {
         return f32(self, b);
       }
       builder f36([int, int] self, builder b) {
         builder b = f21(first(self), b);
         builder b = f23(second(self), b);
         return b;
       }
       builder f35([int, int] self, builder b) {
         return f36(self, b);
       }
       builder f25([[int, int, int], [tuple, tuple], [int, int], [int, int]]
           self, builder b) {
         builder b = f26(first(self), b);
         builder b = f29(second(self), b);
         builder b = f31(third(self), b);
         builder b = f35(fourth(self), b);
         return b;
       }
       builder f24([[int, int, int], [tuple, tuple], [int, int], [int, int]]
           self, builder b) {
         return f25(self, b);
       }
       builder f2(tuple self, builder b) {
         {
         tuple temp = self;
       int discr =
       first(temp);
       if (discr == 1)
       {
         [[int, int, int], [tuple, tuple], [int, int], [int, int]] info =
           second(temp);
       {
         builder b = f3(b, 0, 1);
       return
       f24(info, b);
       }} else if (discr == 0)
       {
         [tuple, tuple, int, int] info = second(temp);
       {
         builder b = f3(b, 3, 2);
       return
       f4(info, b);
       }} else
       {
         }}}
       builder f37([] self, builder b) {
         return b;
       }
       builder f1([tuple, []] self, builder b) {
         builder b = f2(first(self), b);
         builder b = f3(b, 0, 1);
         builder b = f3(b, 0, 1);
         builder b = f37(second(self), b);
         return b;
       }
       _ test([tuple, []] m) {
         builder b = f0();
         builder b = f1(m, b);
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
       int builtin_equal(int i1, int i2) {
         return __==__(i1, i2);
       }
       _ builtin_throw(int e) {
         return throw(e);
       }
       _ builtin_send_raw_msg(cell c, int f) {
         return send_raw_msg(c, f);
       }
       (int, int) builtin_divmod(int i1, int i2) {
         return divmod(i1, i2);
       }
       _ builtin_end_parse(slice s) {
         return end_parse(s);
       }
       (slice, int) builtin_load_coins(slice s) {
         return load_coins(s);
       }
       (slice, slice) builtin_load_bits(slice s, int bs) {
         return load_bits(s, bs);
       }
       (slice, int) builtin_load_uint(slice s, int bs) {
         return load_uint(s, bs);
       }
       (slice, int) builtin_load_int(slice s, int bs) {
         return load_int(s, bs);
       }
       slice builtin_begin_parse(cell c) {
         return begin_parse(c);
       }
       builder builtin_store_coins(builder b, int c) {
         return store_coins(b, c);
       }
       builder builtin_store_uint(builder b, int i, int bs) {
         return store_uint(b, i, bs);
       }
       builder builtin_store_int(builder b, int i, int bs) {
         return store_int(b, i, bs);
       }
       cell builtin_end_cell(builder b) {
         return end_cell(b);
       }
       builder builtin_begin_cell() {
         return begin_cell();
       }
       _ throw(int n) {
         return builtin_throw(n);
       }
       _ send_raw_msg(cell msg, int flags) {
         return builtin_send_raw_msg(msg, flags);
       }
       slice f0(cell cell_) {
         return builtin_begin_parse(cell_);
       }
       [slice, int] f3(slice self, int bits) {
         (slice, int) output = builtin_load_int(self, bits);
         slice slice_ = tensor2_value1(output);
         int int_ = tensor2_value2(output);
         return [slice_, int_];
       }
       [slice, int] f8(slice s) {
         [slice, int] res = f3(s, 9);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int]] f7(slice slice_) {
         [slice slice_, int len] = f8(slice_);
         [slice slice_, int bits] = f3(slice_, len);
         return [slice_, [len, bits]];
       }
       [slice, tuple] f9(slice s, tuple v) {
         return [s, v];
       }
       [slice, tuple] f6(slice s) {
         [slice, int] res_discr = f3(s, 1);
         if (builtin_equal(second(res_discr), 0)) {
         return f9(first(res_discr), []);
       } else if (builtin_equal(second(res_discr), 1))
       {
         [slice, [int, int]] res_addr = f7(first(res_discr));
       return
       f9(first(res_addr), second(res_addr));
       } else
       {
         throw(0);
       }}
       [slice, int] f12(slice s) {
         [slice, int] res = f3(s, 8);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int, int]] f13(slice s, [int, int, int] v) {
         return [s, v];
       }
       [slice, [int, int, int]] f11(slice s) {
         [slice, int] res_anycast = f3(s, 1);
         if (builtin_equal(second(res_anycast), 0)) {
         [slice, int] res_len = f8(first(res_anycast));
       [slice, int] res_workchain =
       f12(first(res_len));
       [slice, int] res_address =
       f3(first(res_workchain), res_len);
       return
       f13(first(res_address), [second(res_len), second(res_workchain), second(res_address)]);
       } else
       {
         throw(0);
       }}
       [slice, tuple] f14(slice s, tuple v) {
         return [s, v];
       }
       [slice, int] f17(slice s) {
         [slice, int] res = f3(s, 256);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int]] f16(slice slice_) {
         [slice slice_, int workchain_id] = f12(slice_);
         [slice slice_, int address] = f17(slice_);
         return [[workchain_id, address], slice_];
       }
       [slice, [int, int]] f15(slice s) {
         [slice, int] res_anycast = f3(s, 1);
         if (builtin_equal(second(res_anycast), 0)) {
         return f16(s);
       } else
       {
         throw(0);
       }}
       [slice, tuple] f10(slice s) {
         [slice, int] res_discr = f3(s, 1);
         if (builtin_equal(second(res_discr), 0)) {
         [slice, [int, int]] res_addr = f15(first(res_discr));
       return
       f14(first(res_addr), second(res_addr));
       } else
       {
         [slice, [int, int, int]] res_addr = f11(first(res_discr));
       return
       f14(first(res_addr), second(res_addr));
       }}
       [slice, int] f19(slice self) {
         (slice, int) output = builtin_load_coins(self);
         slice slice_ = tensor2_value1(output);
         int coins = tensor2_value2(output);
         return [slice_, coins];
       }
       [slice, int] f20(slice s, int v) {
         return [s, v];
       }
       [slice, int] f18(slice s) {
         [slice slice_, int value] = f19(s);
         return f20(value, slice_);
       }
       [slice, [tuple, tuple, int]] f5(slice slice_) {
         [slice slice_, tuple src] = f6(slice_);
         [slice slice_, tuple dest] = f10(slice_);
         [slice slice_, int import_fee] = f18(slice_);
         return [[src, dest, import_fee], slice_];
       }
       [slice, [tuple, tuple, int]] f4(slice s) {
         return f5(s);
       }
       [slice, tuple] f21(slice s, tuple v) {
         return [s, v];
       }
       [slice, int] f26(slice s) {
         [slice, int] res = f3(s, 1);
         [slice slice_, int value] = res;
         return [slice_, value];
       }
       [slice, [int, int, int]] f25(slice slice_) {
         [slice slice_, int ihr_disabled] = f26(slice_);
         [slice slice_, int bounce] = f26(slice_);
         [slice slice_, int bounced] = f26(slice_);
         return [[ihr_disabled, bounce, bounced], slice_];
       }
       [slice, [int, int, int]] f24(slice s) {
         return f25(s);
       }
       [slice, [tuple, tuple]] f28(slice slice_) {
         [slice slice_, tuple src] = f10(slice_);
         [slice slice_, tuple dst] = f10(slice_);
         return [[src, dst], slice_];
       }
       [slice, [tuple, tuple]] f27(slice s) {
         return f28(s);
       }
       [slice, [int, int]] f30(slice slice_) {
         [slice slice_, int ihr_fee] = f18(slice_);
         [slice slice_, int fwd_fee] = f18(slice_);
         return [[ihr_fee, fwd_fee], slice_];
       }
       [slice, [int, int]] f29(slice s) {
         return f30(s);
       }
       [slice, int] f34(slice self, int bits) {
         (slice, int) output = builtin_load_uint(self, bits);
         slice slice_ = tensor2_value1(output);
         int int_ = tensor2_value2(output);
         return [slice_, int_];
       }
       [slice, int] f33(slice s) {
         [slice, int] res = f34(s, 64);
         return [first(res), second(res)];
       }
       [slice, int] f35(slice s) {
         [slice, int] res = f34(s, 32);
         return [first(res), second(res)];
       }
       [slice, [int, int]] f32(slice slice_) {
         [slice slice_, int created_lt] = f33(slice_);
         [slice slice_, int created_at] = f35(slice_);
         return [[created_lt, created_at], slice_];
       }
       [slice, [int, int]] f31(slice s) {
         return f32(s);
       }
       [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] f23(slice
           slice_) {
         [slice slice_, [int, int, int] flags] = f24(slice_);
         [slice slice_, [tuple, tuple] addresses] = f27(slice_);
         [slice slice_, [int, int] coins] = f29(slice_);
         [slice slice_, [int, int] timestamps] = f31(slice_);
         return [[flags, addresses, coins, timestamps], slice_];
       }
       [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] f22(slice s)
           {
         return f23(s);
       }
       [slice, tuple] f2(slice s) {
         [slice, int] res_discr1 = f3(s, 1);
         if (builtin_equal(second(res_discr1), 0)) {
         [slice, [[int, int, int], [tuple, tuple], [int, int], [int, int]]] res_info =
           f22(first(res_discr1));
       return
       f21(first(res_info), second(res_info));
       } else
       {
         [slice, int] res_discr2 = f3(first(res_discr1), 1);
       if (builtin_equal(second(res_discr2), 0))
       {
         [slice, [tuple, tuple, int]] res_info = f4(first(res_discr2));
       return
       f21(first(res_info), second(res_info));
       } else
       {
         throw(0);
       }}}
       [slice, []] f37(slice s, [] v) {
         return [s, v];
       }
       [slice, []] f36(slice s) {
         return f37(s, []);
       }
       [slice, [tuple, []]] f38(slice s, [tuple, []] v) {
         return [s, v];
       }
       [slice, [tuple, []]] f1(slice s) {
         [slice, tuple] res_info = f2(s);
         [slice, int] res_init = f3(first(res_info), 1);
         if (builtin_equal(second(res_init), 0)) {
         [slice, int] res_body_discr = f3(first(res_init), 1);
       if (builtin_equal(second(res_body_discr), 0))
       {
         [slice, []] body = f36(first(res_body_discr));
       [tuple, [tuple, []]] mes =
       [second(res_info), second(body)];
       return
       f38(first(body), mes);
       } else
       {
         }} else
       {
         throw(0);
       }}
       _ test(cell c) {
         slice s = f0(c);
         [slice, [tuple, []]] msg = f1(s);
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
       int test([int, int, int] t) {
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
       int test([int, int, int] t) {
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
    int builtin_equal(int i1, int i2) {
      return __==__(i1, i2);
    }
    _ builtin_throw(int e) {
      return throw(e);
    }
    _ builtin_send_raw_msg(cell c, int f) {
      return send_raw_msg(c, f);
    }
    (int, int) builtin_divmod(int i1, int i2) {
      return divmod(i1, i2);
    }
    _ builtin_end_parse(slice s) {
      return end_parse(s);
    }
    (slice, int) builtin_load_coins(slice s) {
      return load_coins(s);
    }
    (slice, slice) builtin_load_bits(slice s, int bs) {
      return load_bits(s, bs);
    }
    (slice, int) builtin_load_uint(slice s, int bs) {
      return load_uint(s, bs);
    }
    (slice, int) builtin_load_int(slice s, int bs) {
      return load_int(s, bs);
    }
    slice builtin_begin_parse(cell c) {
      return begin_parse(c);
    }
    builder builtin_store_coins(builder b, int c) {
      return store_coins(b, c);
    }
    builder builtin_store_uint(builder b, int i, int bs) {
      return store_uint(b, i, bs);
    }
    builder builtin_store_int(builder b, int i, int bs) {
      return store_int(b, i, bs);
    }
    cell builtin_end_cell(builder b) {
      return end_cell(b);
    }
    builder builtin_begin_cell() {
      return begin_cell();
    }
    _ throw(int n) {
      return builtin_throw(n);
    }
    _ send_raw_msg(cell msg, int flags) {
      return builtin_send_raw_msg(msg, flags);
    }
    [slice, int] f1(slice self, int bits) {
      (slice, int) output = builtin_load_int(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [slice_, int_];
    }
    [slice, int] f0(slice s) {
      [slice, int] res = f1(s, 9);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, int] f2(slice s) {
      [slice, int] res = f1(s, 256);
      [slice slice_, int value] = res;
      return [slice_, value];
    }
    [slice, [int, int]] test(slice slice_) {
      [slice slice_, int value1] = f0(slice_);
      [slice slice_, int value2] = f2(slice_);
      return [[value1, value2], slice_];
    } |}]
