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
    ?(bindings = Lang.default_bindings) ?(structs = Lang.default_structs)
    ?(strip_defaults = false) p =
  let c = new Lang.constructor bindings structs [] errors in
  let p' = c#visit_program () p in
  errors#to_result p'
  (* remove default bindings and methods *)
  |> Result.map ~f:(fun (program : Lang.program) ->
         if strip_defaults then
           { program with
             bindings =
               List.filter program.bindings ~f:(fun binding ->
                   not @@ List.exists bindings ~f:(Lang.equal_binding binding) );
             structs =
               List.filter program.structs ~f:(fun (id1, _) ->
                   not
                   @@ List.exists structs ~f:(fun (id2, _) -> equal_int id1 id2) )
           }
         else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> (err, p')) )

exception Exn of error list

let pp ?(bindings = Lang.default_bindings) s =
  parse_program s |> build_program ~bindings
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn |> Codegen.codegen
  |> Func.pp_program Caml.Format.std_formatter

let%expect_test "simple function generation" =
  let source = {|
      fn test() -> Integer { return 0; }
    |} in
  pp source ; [%expect {|
    int test() {
      return 0;
    } |}]

let%expect_test "passing struct to function" =
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
  [%expect {|
    int test([int, int, int] t) {
      return 1;
    } |}]

let%expect_test "function calls " =
  let source =
    {|
      fn test(value: Integer) -> Integer { return value; }
      fn test2(value: Integer) -> Integer { return test(value); }
    |}
  in
  pp source ;
  [%expect
    {|
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
    builder f0(int self, builder b) {
      return store_int(b, self, 32);
    }
    _ test_int(builder b) {
      int i = 100;
      f0(100, b);
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
    builder f0(int self, builder b) {
      return store_int(b, self, 32);
    }
    builder T_serializer([int, int] self, builder b) {
      builder b = f0(first(self), b);
      builder b = f0(second(self), b);
      return b;
    }
    builder f1() {
      return new_builder();
    }
    _ test() {
      builder b = f1();
      T_serializer([0, 1], b);
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
    builder f0(int self, builder b) {
      return store_int(b, self, 32);
    }
    builder serialize_foo([int, int] self, builder b) {
      builder b = f0(first(self), b);
      builder b = f0(second(self), b);
      return b;
    }
    builder f1() {
      return new_builder();
    }
    builder test() {
      builder b = f1();
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
  pp source ;
  [%expect
    {|
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
    builder f0(int self, builder b) {
      return store_int(b, self, 160);
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
