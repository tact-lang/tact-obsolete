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
  let c = new Lang.constructor ~program:prev_program errors in
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
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
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
  [%expect
    {|
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
    }
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
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Not_found_s "List.Assoc.find_exn: not found")
  Raised at Base__List.Assoc.find_exn.find_exn in file "src/list.ml" (inlined), line 1068, characters 16-31
  Called from Tact__Type_check.type_checker#check_type in file "lib/type_check.ml", line 55, characters 12-68
  Called from Tact__Lang.Make.constructor#build_FunctionCall.(fun) in file "lib/lang.ml", line 112, characters 26-63
  Called from Stdlib__List.rev_map2.rmap2_f in file "list.ml", line 138, characters 35-42
  Called from Base__List0.rev_map2_ok in file "src/list0.ml" (inlined), line 31, characters 27-54
  Called from Base__List.map2_ok in file "src/list.ml" (inlined), line 515, characters 27-49
  Called from Base__List.map2 in file "src/list.ml" (inlined), line 516, characters 43-55
  Called from Base__List.map2 in file "src/list.ml", line 516, characters 43-55
  Called from Base__List.check_length2 in file "src/list.ml", line 176, characters 43-52
  Called from Tact__Lang.Make.constructor#build_FunctionCall in file "lib/lang.ml", line 110, characters 16-616
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.visitor#visit_Expr in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Syntax.Make.visitor#visit_CodeBlock in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Syntax.Make.visitor#visit_function_body in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_function_body in file "lib/lang.ml", line 354, characters 23-57
  Called from VisitorsRuntime.map#visit_option in file "runtime/VisitorsRuntime.ml", line 278, characters 15-24
  Called from Tact__Type_check.type_checker#with_fn_returns in file "lib/type_check.ml", line 36, characters 21-26
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Lang.Make.constructor#visit_function_definition in file "lib/lang.ml", line 335, characters 12-240
  Called from Tact__Syntax.Make.visitor#visit_Function in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_binding in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_Let in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Syntax.Make.visitor#visit_program in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact_tests__Codegen_func.build_program in file "test/codegen_func.ml", line 22, characters 11-31
  Called from Tact_tests__Codegen_func.pp in file "test/codegen_func.ml", line 34, characters 2-50
  Called from Tact_tests__Codegen_func.(fun) in file "test/codegen_func.ml", line 113, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

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
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Not_found_s "List.Assoc.find_exn: not found")
  Raised at Base__List.Assoc.find_exn.find_exn in file "src/list.ml" (inlined), line 1068, characters 16-31
  Called from Tact__Type_check.type_checker#check_type in file "lib/type_check.ml", line 55, characters 12-68
  Called from Tact__Lang.Make.constructor#build_FunctionCall.(fun) in file "lib/lang.ml", line 112, characters 26-63
  Called from Stdlib__List.rev_map2.rmap2_f in file "list.ml", line 138, characters 35-42
  Called from Base__List0.rev_map2_ok in file "src/list0.ml" (inlined), line 31, characters 27-54
  Called from Base__List.map2_ok in file "src/list.ml" (inlined), line 515, characters 27-49
  Called from Base__List.map2 in file "src/list.ml" (inlined), line 516, characters 43-55
  Called from Base__List.map2 in file "src/list.ml", line 516, characters 43-55
  Called from Base__List.check_length2 in file "src/list.ml", line 176, characters 43-52
  Called from Tact__Lang.Make.constructor#build_FunctionCall in file "lib/lang.ml", line 110, characters 16-616
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.visitor#visit_Expr in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Syntax.Make.visitor#visit_CodeBlock in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Syntax.Make.visitor#visit_function_body in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_function_body in file "lib/lang.ml", line 354, characters 23-57
  Called from VisitorsRuntime.map#visit_option in file "runtime/VisitorsRuntime.ml", line 278, characters 15-24
  Called from Tact__Type_check.type_checker#with_fn_returns in file "lib/type_check.ml", line 36, characters 21-26
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Lang.Make.constructor#visit_function_definition in file "lib/lang.ml", line 335, characters 12-240
  Called from Tact__Syntax.Make.visitor#visit_Function in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_binding in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_Let in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Syntax.Make.visitor#visit_program in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact_tests__Codegen_func.build_program in file "test/codegen_func.ml", line 22, characters 11-31
  Called from Tact_tests__Codegen_func.pp in file "test/codegen_func.ml", line 34, characters 2-50
  Called from Tact_tests__Codegen_func.(fun) in file "test/codegen_func.ml", line 173, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

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
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Not_found_s "List.Assoc.find_exn: not found")
  Raised at Base__List.Assoc.find_exn.find_exn in file "src/list.ml" (inlined), line 1068, characters 16-31
  Called from Tact__Type_check.type_checker#check_type in file "lib/type_check.ml", line 55, characters 12-68
  Called from Tact__Lang.Make.constructor#build_FunctionCall.(fun) in file "lib/lang.ml", line 112, characters 26-63
  Called from Stdlib__List.rev_map2.rmap2_f in file "list.ml", line 138, characters 35-42
  Called from Base__List0.rev_map2_ok in file "src/list0.ml" (inlined), line 31, characters 27-54
  Called from Base__List.map2_ok in file "src/list.ml" (inlined), line 515, characters 27-49
  Called from Base__List.map2 in file "src/list.ml" (inlined), line 516, characters 43-55
  Called from Base__List.map2 in file "src/list.ml", line 516, characters 43-55
  Called from Base__List.check_length2 in file "src/list.ml", line 176, characters 43-52
  Called from Tact__Lang.Make.constructor#build_FunctionCall in file "lib/lang.ml", line 110, characters 16-616
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.visitor#visit_Return in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Syntax.Make.visitor#visit_CodeBlock in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Syntax.Make.visitor#visit_function_body in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_function_body in file "lib/lang.ml", line 354, characters 23-57
  Called from VisitorsRuntime.map#visit_option in file "runtime/VisitorsRuntime.ml", line 278, characters 15-24
  Called from Tact__Type_check.type_checker#with_fn_returns in file "lib/type_check.ml", line 36, characters 21-26
  Called from Tact__Lang.Make.constructor#with_bindings in file "lib/lang.ml", line 542, characters 25-29
  Called from Tact__Lang.Make.constructor#visit_function_definition in file "lib/lang.ml", line 335, characters 12-240
  Called from Tact__Syntax.Make.visitor#visit_Function in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Lang.Make.constructor#visit_expr in file "lib/lang.ml", line 255, characters 22-54
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_binding in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from Tact__Syntax.Make.visitor#visit_Let in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact__Syntax.Make.base_visitor#visit_located in file "lib/syntax.ml", line 19, characters 45-62
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Syntax.Make.visitor#visit_program in file "lib/syntax.ml", line 22, characters 4-1023
  Called from Tact_tests__Codegen_func.build_program in file "test/codegen_func.ml", line 22, characters 11-31
  Called from Tact_tests__Codegen_func.pp in file "test/codegen_func.ml", line 34, characters 2-50
  Called from Tact_tests__Codegen_func.(fun) in file "test/codegen_func.ml", line 235, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

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
      cell builtin_builder_build(builder b) {
        return build(b);
      }
      builder builtin_builder_new() {
        return new_builder();
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
      cell builtin_builder_build(builder b) {
        return build(b);
      }
      builder builtin_builder_new() {
        return new_builder();
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
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
    cell builtin_builder_build(builder b) {
      return build(b);
    }
    builder builtin_builder_new() {
      return new_builder();
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
