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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int f0(int i) {
      i;
    }
    builder f2(builder self, int int, int bits) {
      builder b = builtin_builder_store_int(self, int, bits);
      b;
    }
    builder f1(int self, builder builder) {
      f2(builder, self, 32);
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int, int bits) {
      builder b = builtin_builder_store_int(self, int, bits);
      b;
    }
    builder f0(int self, builder builder) {
      f1(builder, self, 32);
    }
    builder f2(int self, builder builder) {
      f1(builder, self, 16);
    }
    builder T_serializer([int, int] self, builder b) {
      builder b = f0(first(self), b);
      builder b = f2(second(self), b);
      return b;
    }
    builder f3() {
      builtin_builder_new();
    }
    int f4(int i) {
      i;
    }
    int f5(int i) {
      i;
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int, int bits) {
      builder b = builtin_builder_store_int(self, int, bits);
      b;
    }
    builder f0(int self, builder builder) {
      f1(builder, self, 32);
    }
    builder f2(int self, builder builder) {
      f1(builder, self, 16);
    }
    builder serialize_foo([int, int] self, builder b) {
      builder b = f0(first(self), b);
      builder b = f2(second(self), b);
      return b;
    }
    builder f3() {
      builtin_builder_new();
    }
    int f4(int i) {
      i;
    }
    int f5(int i) {
      i;
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
      _ builtin_send_raw_msg(cell msg, int flags) {
        return send_raw_message(msg, flags);
      }
      builder builtin_builder_store_int(builder b, int int, int bits) {
        return store_int(b, int, bits);
      }
      cell builtin_builder_build(builder b) {
        return build(b);
      }
      builder builtin_builder_new() {
        return new_builder();
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
      _ builtin_send_raw_msg(cell msg, int flags) {
        return send_raw_message(msg, flags);
      }
      builder builtin_builder_store_int(builder b, int int, int bits) {
        return store_int(b, int, bits);
      }
      cell builtin_builder_build(builder b) {
        return build(b);
      }
      builder builtin_builder_new() {
        return new_builder();
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    builder f1(builder self, int int, int bits) {
      builder b = builtin_builder_store_int(self, int, bits);
      b;
    }
    builder f0(int self, builder builder) {
      f1(builder, self, 32);
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    tuple try(tuple x) {
      x;
    }
    tuple f0(int v) {
      return [1, v];
    }
    tuple f1([] v) {
      return [0, v];
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
    _ builtin_send_raw_msg(cell msg, int flags) {
      return send_raw_message(msg, flags);
    }
    builder builtin_builder_store_int(builder b, int int, int bits) {
      return store_int(b, int, bits);
    }
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
    _ send_raw_msg(cell msg, int flags) {
      builtin_send_raw_msg(msg, flags);
    }
    int test(tuple i) {
      {
      tuple temp = i;
    int discr =
    first(temp);
    if (discr == 1)
    {
      int vax = second(temp);
    {
      return 32;
    }} else if (discr == 0)
    {
      int vax = second(temp);
    {
      return 64;
    }} else
    {
      }}} |}]
