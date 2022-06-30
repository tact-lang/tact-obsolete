module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Show = Tact.Show.Make (Syntax)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
module Codegen = Tact.Codegen_func
module Func = Tact.Func
include Core

type error = [Lang.error | Interpreter.error] * Lang.program
[@@deriving sexp_of]

let make_errors e = new Errors.errors e

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors Show.show_error)
    ?(prev_program = Lang.default_program ()) ?(strip_defaults = true) p =
  let std =
    let c = new Lang.constructor ~program:prev_program errors in
    let p' = c#visit_program () (parse_program Tact.Builtin.std) in
    p'
  in
  let c = new Lang.constructor ~program:std errors in
  let p' = c#visit_program () p in
  errors#to_result ()
  |> Result.map ~f:(fun _ -> p')
  (* remove default bindings and methods *)
  |> Result.map ~f:(fun (program : Lang.program) ->
         if strip_defaults then program else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> (err, p')) )

exception Exn of error list

let pp ?(prev_program = Lang.default_program ()) s =
  parse_program s
  |> build_program ~prev_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn |> Codegen.codegen
  |> Func.pp_program Caml.Format.std_formatter

let%expect_test "simple function generation" =
  let source = {|
      fn test() -> Integer { return 0; }
    |} in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int test() {
      return 0;
    } |}]

let%expect_test "passing struct to a function" =
  let source =
    {|
      struct T { 
       val a: Int(32)
       val b: Integer
       val c: struct { val d : Integer }
      }
      fn test(t: T) -> Integer { return 1; }
    |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int test([int, int, int] t) {
      return 1;
    } |}]

let%expect_test "function calls" =
  let source =
    {|
      fn test(value: Integer) -> Integer { return value; }
      fn test2(value: Integer) -> Integer { return test(value); }
    |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int test(int value) {
      return value;
    }
    int test2(int value) {
      return test(value);
    } |}]

let%expect_test "Int(bits) serializer codegen" =
  let source =
    {|
        fn test_int(b: Builder) {
          let i = Int(32).new(100);
          i.serialize(b);
        }
      |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int f0(int i) {
      return i;
    }
    builder f2(builder self, int int_, int bits) {
      builder b = builtin_builder_store_int(self, int_, bits);
      return b;
    }
    builder f1(int self, builder builder_) {
      return f2(builder_, self, 32);
    }
    _ test_int(builder b) {
      int i = f0(100);
      f1(i, b);
    } |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
        struct T {
          val a: Int(32)
          val b: Int(16)
        }
        let T_serializer = serializer(T);
  
        fn test() {
          let b = Builder.new();
          T_serializer(T{a: Int(32).new(0), b: Int(16).new(1)}, b);
        }
      |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int_, int bits) {
      builder b = builtin_builder_store_int(self, int_, bits);
      return b;
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
      return builtin_builder_new();
    }
    int f4(int i) {
      return i;
    }
    int f5(int i) {
      return i;
    }
    _ test() {
      builder b = f3();
      T_serializer([f4(0), f5(1)], b);
    } |}]

let%expect_test "demo struct serializer 2" =
  let source =
    {|
      struct Foo {
        val a: Int(32)
        val b: Int(16)
      }
      let serialize_foo = serializer(Foo);

      fn test() -> Builder {
        let b = Builder.new();
        return serialize_foo(Foo{a: Int(32).new(0), b: Int(16).new(1)}, b);
      }
    |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int_, int bits) {
      builder b = builtin_builder_store_int(self, int_, bits);
      return b;
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
      return builtin_builder_new();
    }
    int f4(int i) {
      return i;
    }
    int f5(int i) {
      return i;
    }
    builder test() {
      builder b = f3();
      return serialize_foo([f4(0), f5(1)], b);
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
  pp source ;
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
      int builtin_equal(int x, int y) {
        return x == y;
      }
      _ builtin_send_raw_msg(cell msg, int flags) {
        return send_raw_message(msg, flags);
      }
      (int, int) builtin_divmod(int x, int y) {
        return divmod(x, y);
      }
      (slice, int) builtin_slice_load_int(slice s, int bits) {
        return load_int(s, bits);
      }
      _ builtin_slice_end_parse(slice s) {
        return end_parse(s);
      }
      slice builtin_slice_begin_parse(cell c) {
        return begin_parse(c);
      }
      int builtin_builder_store_coins(builder b, int c) {
        return store_grams(b, c);
      }
      builder builtin_builder_store_int(builder b, int int_, int bits) {
        return store_int(b, int_, bits);
      }
      cell builtin_builder_build(builder b) {
        return end_cell(b);
      }
      builder builtin_builder_new() {
        return begin_cell();
      }
      _ send_raw_msg(cell msg, int flags) {
        builtin_send_raw_msg(msg, flags);
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
  pp source ;
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
      int builtin_equal(int x, int y) {
        return x == y;
      }
      _ builtin_send_raw_msg(cell msg, int flags) {
        return send_raw_message(msg, flags);
      }
      (int, int) builtin_divmod(int x, int y) {
        return divmod(x, y);
      }
      (slice, int) builtin_slice_load_int(slice s, int bits) {
        return load_int(s, bits);
      }
      _ builtin_slice_end_parse(slice s) {
        return end_parse(s);
      }
      slice builtin_slice_begin_parse(cell c) {
        return begin_parse(c);
      }
      int builtin_builder_store_coins(builder b, int c) {
        return store_grams(b, c);
      }
      builder builtin_builder_store_int(builder b, int int_, int bits) {
        return store_int(b, int_, bits);
      }
      cell builtin_builder_build(builder b) {
        return end_cell(b);
      }
      builder builtin_builder_new() {
        return begin_cell();
      }
      _ send_raw_msg(cell msg, int flags) {
        builtin_send_raw_msg(msg, flags);
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
      struct Pubkey { val x: Int(160) }
      struct Wallet { val seqno: Int(32) val pubkey: Pubkey }
      let serialize_wallet = serializer(Wallet);
    |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int_, int bits) {
      builder b = builtin_builder_store_int(self, int_, bits);
      return b;
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
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    tuple try(tuple x) {
      return x;
    }
    tuple f0(int v) {
      return [0, v];
    }
    tuple f1([] v) {
      return [1, v];
    }
    _ test_try(int x, [] y) {
      tuple test1 = try(f0(x));
      tuple test2 = try(f1(y));
    } |}]

let%expect_test "switch statement" =
  let source =
    {|
      union Ints {
        case Int(32)
        case Int(64)
      }
      fn test(i: Ints) -> Integer {
        switch (i) {
          case Int(32) vax => { return 32; }
          case Int(64) vax => { return 64; }
        }
      }
    |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
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
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
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
     fn test(m: MessageRelaxed(Empty)) {
       let b = Builder.new();
       let b = m.serialize(b);
     }
     |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f0() {
      return builtin_builder_new();
    }
    builder f3(builder self, int int_, int bits) {
      builder b = builtin_builder_store_int(self, int_, bits);
      return b;
    }
    builder f11(int self, builder builder_) {
      return f3(builder_, self, 9);
    }
    builder f12(int self, builder builder_) {
      return f3(builder_, self, 8);
    }
    builder f10([int, int, int] self, builder b) {
      builder b = f11(first(self), b);
      builder b = f12(second(self), b);
      return b;
    }
    builder f9([int, int, int] self, builder b) {
      builder b = f3(b, 0, 0);
      builder b = f10(self, b);
      return b;
    }
    builder f15(int self, builder builder_) {
      return f3(builder_, self, 256);
    }
    builder f14([int, int] self, builder b) {
      builder b = f12(first(self), b);
      builder b = f15(second(self), b);
      return b;
    }
    builder f13([int, int] self, builder b) {
      builder b = f3(b, 0, 0);
      return f14(self, b);
    }
    builder f8(tuple self, builder b) {
      {
      tuple temp = self;
    int discr =
    first(temp);
    if (discr == 0)
    {
      [int, int] var = second(temp);
    {
      int b = store_uint(b, 1, 1);
    builder b =
    f13(var, b);
    return
    b;
    }} else if (discr == 1)
    {
      [int, int, int] var = second(temp);
    {
      int b = store_uint(b, 0, 1);
    builder b =
    f9(var, b);
    return
    b;
    }} else
    {
      }}}
    builder f7(tuple self, builder b) {
      return f8(self, b);
    }
    builder f18([int, int] self, builder b) {
      builder b = f11(first(self), b);
      builder b = f3(b, second(self), first(self));
      return b;
    }
    builder f19([] self, builder b) {
      return b;
    }
    builder f17(tuple self, builder b) {
      {
      tuple temp = self;
    int discr =
    first(temp);
    if (discr == 0)
    {
      [] var = second(temp);
    {
      int b = store_uint(b, 1, 1);
    builder b =
    f19(var, b);
    return
    b;
    }} else if (discr == 1)
    {
      [int, int] var = second(temp);
    {
      int b = store_uint(b, 0, 1);
    builder b =
    f18(var, b);
    return
    b;
    }} else
    {
      }}}
    builder f16(tuple self, builder b) {
      return f17(self, b);
    }
    builder f6(tuple self, builder b) {
      {
      tuple temp = self;
    int discr =
    first(temp);
    if (discr == 0)
    {
      tuple var = second(temp);
    {
      int b = store_uint(b, 1, 1);
    builder b =
    f16(var, b);
    return
    b;
    }} else if (discr == 1)
    {
      tuple var = second(temp);
    {
      int b = store_uint(b, 0, 1);
    builder b =
    f7(var, b);
    return
    b;
    }} else
    {
      }}}
    builder f5(tuple self, builder b) {
      return f6(self, b);
    }
    builder f20(int self, builder builder_) {
      return f3(builder_, self, 64);
    }
    builder f21(int self, builder builder_) {
      return f3(builder_, self, 32);
    }
    builder f4([tuple, tuple, int, int] self, builder b) {
      builder b = f5(first(self), b);
      builder b = f16(second(self), b);
      builder b = f20(third(self), b);
      builder b = f21(fourth(self), b);
      return b;
    }
    builder f26(int self, builder builder_) {
      return f3(builder_, self, 1);
    }
    builder f25([int, int, int] self, builder b) {
      builder b = f26(first(self), b);
      builder b = f26(second(self), b);
      builder b = f26(third(self), b);
      return b;
    }
    builder f24([int, int, int] self, builder b) {
      return f25(self, b);
    }
    builder f28([tuple, tuple] self, builder b) {
      builder b = f7(first(self), b);
      builder b = f7(second(self), b);
      return b;
    }
    builder f27([tuple, tuple] self, builder b) {
      return f28(self, b);
    }
    builder f32(builder self, int c) {
      int b = builtin_builder_store_coins(self, c);
      return b;
    }
    builder f31(int self, builder builder_) {
      return f32(builder_, self);
    }
    builder f30([int, int] self, builder b) {
      builder b = f31(first(self), b);
      builder b = f31(second(self), b);
      return b;
    }
    builder f29([int, int] self, builder b) {
      return f30(self, b);
    }
    builder f34([int, int] self, builder b) {
      builder b = f20(first(self), b);
      builder b = f21(second(self), b);
      return b;
    }
    builder f33([int, int] self, builder b) {
      return f34(self, b);
    }
    builder f23([[int, int, int], [tuple, tuple], [int, int], [int, int]]
        self, builder b) {
      builder b = f24(first(self), b);
      builder b = f27(second(self), b);
      builder b = f29(third(self), b);
      builder b = f33(fourth(self), b);
      return b;
    }
    builder f22([[int, int, int], [tuple, tuple], [int, int], [int, int]]
        self, builder b) {
      return f23(self, b);
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
    f22(info, b);
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
    builder f35([] self, builder b) {
      return b;
    }
    builder f1([tuple, []] self, builder b) {
      builder b = f2(first(self), b);
      builder b = f3(b, 0, 1);
      builder b = f3(b, 0, 1);
      builder b = f35(second(self), b);
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
        fn deserialize(s: Slice) -> LoadResult(Self) {
          return LoadResult(Self).new(s, Self{});
        }
      }
    }
     fn test(c: Cell) {
       let s = Slice.parse(c);
       let msg = Message(Empty).deserialize(s);
     }
     |}
  in
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    slice f0(cell cell_) {
      return builtin_slice_begin_parse(cell_);
    }
    [slice, int] f3(slice self, int bits) {
      (slice, int) output = builtin_slice_load_int(self, bits);
      slice slice_ = tensor2_value1(output);
      int int_ = tensor2_value2(output);
      return [slice_, int_];
    }
    [slice, int] f7(slice s) {
      [slice, int] res = f3(s, 9);
      return [first(res), second(res)];
    }
    [slice, int] f8(slice s) {
      [slice, int] res = f3(s, 8);
      return [first(res), second(res)];
    }
    [slice, [int, int, int]] f9(slice s, [int, int, int] v) {
      return [s, v];
    }
    [slice, [int, int, int]] f6(slice s) {
      [slice, int] res_anycast = f3(s, 1);
      if (builtin_equal(second(res_anycast), 0)) {
      [slice, int] res_len = f7(first(res_anycast));
    [slice, int] res_workchain =
    f8(first(res_len));
    [slice, int] res_address =
    f3(first(res_workchain), res_len);
    return
    f9(first(res_address), [second(res_len), second(res_workchain), second(res_address)]);
    } else
    {
      }}
    [slice, tuple] f10(slice s, tuple v) {
      return [s, v];
    }
    [slice, int] f12(slice s) {
      [slice, int] res = f3(s, 256);
      return [first(res), second(res)];
    }
    [slice, [int, int]] f13(slice s, [int, int] v) {
      return [s, v];
    }
    [slice, [int, int]] f11(slice s) {
      [slice, int] res_anycast = f3(s, 1);
      if (builtin_equal(second(res_anycast), 0)) {
      [slice, int] res_workchain = f8(first(res_anycast));
    [slice, int] res_address =
    f12(first(res_workchain));
    return
    f13(first(res_address), [second(res_workchain), second(res_address)]);
    } else
    {
      }}
    [slice, tuple] f5(slice s) {
      [slice, int] res_discr = f3(s, 1);
      if (builtin_equal(second(res_discr), 0)) {
      [slice, [int, int]] res_addr = f11(second(res_discr));
    return
    f10(first(res_addr), second(res_addr));
    } else
    {
      [slice, [int, int, int]] res_addr = f6(first(res_discr));
    return
    f10(first(res_addr), second(res_addr));
    }}
    [slice, [int, int]] f15(slice s) {
      [slice, int] res_len = f7(s);
      [slice, int] res_bits = f3(first(res_len), second(res_len));
      return [first(res_bits), [second(res_len), second(res_bits)]];
    }
    [slice, tuple] f16(slice s, tuple v) {
      return [s, v];
    }
    [slice, tuple] f14(slice s) {
      [slice, int] res_discr = f3(s, 1);
      if (builtin_equal(second(res_discr), 0)) {
      return f16(first(res_discr), []);
    } else if (builtin_equal(second(res_discr), 1))
    {
      [slice, [int, int]] res_addr = f15(first(res_discr));
    return
    f16(first(res_addr), second(res_addr));
    } else
    {
      }}
    [slice, int] f17(slice s) {
      [slice, int] res = f3(s, 64);
      return [first(res), second(res)];
    }
    [slice, [tuple, tuple, int, int]] f18(slice s, [tuple, tuple, int, int] v) {
      return [s, v];
    }
    [slice, [tuple, tuple, int, int]] f4(slice s) {
      [slice, tuple] res_src = f5(s);
      [slice, tuple] res_dest = f14(first(res_src));
      [slice, int] res_created_lt = f17(first(res_dest));
      [slice, int] res_created_at = f17(first(res_created_lt));
      return
        f18(first(res_created_at), [second(res_src), second(res_dest), second(res_created_lt), second(res_created_at)]);
    }
    [slice, tuple] f19(slice s, tuple v) {
      return [s, v];
    }
    [slice, tuple] f2(slice s) {
      [slice, int] res_discr1 = f3(s, 1);
      if (builtin_equal(second(res_discr1), 0)) {
      } else
    {
      [slice, int] res_discr2 = f3(first(res_discr1), 1);
    if (builtin_equal(second(res_discr2), 0))
    {
      } else
    {
      [slice, [tuple, tuple, int, int]] res_info = f4(first(res_discr2));
    return
    f19(first(res_info), second(res_info));
    }}}
    [slice, []] f21(slice s, [] v) {
      return [s, v];
    }
    [slice, []] f20(slice s) {
      return f21(s, []);
    }
    [slice, [tuple, []]] f22(slice s, [tuple, []] v) {
      return [s, v];
    }
    [slice, [tuple, []]] f1(slice s) {
      [slice, tuple] res_info = f2(s);
      [slice, int] res_init = f3(first(res_info), 1);
      if (builtin_equal(second(res_init), 0)) {
      [slice, int] res_body_discr = f3(first(res_init), 1);
    if (builtin_equal(second(res_body_discr), 0))
    {
      [slice, []] body = f20(first(res_body_discr));
    [tuple, _] mes =
    [second(res_info), second(body)];
    return
    f22(first(body), mes);
    } else
    {
      }} else
    {
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
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
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
  pp source ;
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
    int builtin_equal(int x, int y) {
      return x == y;
    }
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    (int, int) builtin_divmod(int x, int y) {
      return divmod(x, y);
    }
    (slice, int) builtin_slice_load_int(slice s, int bits) {
      return load_int(s, bits);
    }
    _ builtin_slice_end_parse(slice s) {
      return end_parse(s);
    }
    slice builtin_slice_begin_parse(cell c) {
      return begin_parse(c);
    }
    int builtin_builder_store_coins(builder b, int c) {
      return store_grams(b, c);
    }
    builder builtin_builder_store_int(builder b, int int_, int bits) {
      return store_int(b, int_, bits);
    }
    cell builtin_builder_build(builder b) {
      return end_cell(b);
    }
    builder builtin_builder_new() {
      return begin_cell();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int test([int, int, int] t) {
      [_, int y2, _] = t;
      return y2;
    }
 |}]
