module Config = Shared.EnabledConfig
module Show = Tact.Show.Make (Shared.EnabledConfig)
open Config

let fmt = Caml.Format.std_formatter

let%expect_test "error showing one line" =
  let source = {|
fn test() {

}
|} in
  let open Show.DiagnosticMsg in
  let pos1 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 3 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let pos2 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 7 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let msg =
    { severity = `Error;
      diagnostic_id = 0;
      diagnostic_msg = "msg";
      spans = [(span_of_concrete (pos1, pos2), "message")];
      additional_msg = [] }
  in
  Show.DiagnosticMsg.show fmt msg source ;
  [%expect
    {|
    Error[0]: msg
    File: "f":1:3
      |
    1 | fn test() {
      |    ^^^^ message |}]

let%expect_test "error showing two lines" =
  let source = {|
let a = test(
  arg1, arg2);
|} in
  let open Show.DiagnosticMsg in
  let pos1 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 8 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let pos2 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 13 + 13 + 1; pos_lnum = 2; pos_bol = 13 + 1}
  in
  let msg =
    { severity = `Error;
      diagnostic_id = 0;
      diagnostic_msg = "msg";
      spans = [(span_of_concrete (pos1, pos2), "message")];
      additional_msg = [] }
  in
  Show.DiagnosticMsg.show fmt msg source ;
  [%expect
    {|
    Error[0]: msg
    File: "f":1:8
      |
    1 | let a = test(...
      |         ^^^^^^^^ message |}]

let pp =
  let open Base in
  Shared.Enabled.pp_compile ~show_errors:(fun (elist, _) source ->
      List.iter elist ~f:(fun x ->
          let s = Show.show_error source x in
          Caml.Format.print_string s ) )

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    Error[1]: Unresolved identifier Int256
    File: "":2:12
      |
    2 |     let T = Int256;
      |             ^^^^^^ Cannot resolve this identifier |}]

let%expect_test "method not found" =
  let source = {|
      struct St { }
  
      let _ = St{}.method();
    |} in
  pp source ;
  [%expect
    {|
    Error[1]: Method method not found in <anonymous>
    File: "":4:19
      |
    4 |       let _ = St{}.method();
      |                    ^^^^^^ Method not found |}]

let%expect_test "duplicate field" =
  let source =
    {|
      struct Foo {
        val field: Integer
        val field: Integer
      }
    |}
  in
  pp source ;
  [%expect
    {|
    Error[1]: Duplicate struct field field
    File: "":3:12
      |
    3 |         val field: Integer
      |             ^^^^^ Duplicated |}]

let%expect_test "duplicate variant" =
  let source =
    {|
      union Test1 {
        case Integer
        case Integer
      }

      union Test2[T: Type] {
        case Integer
        case T
      }
      let _ = Test2[Integer];
    |}
  in
  pp source ;
  [%expect
    {|
    Error[1]: Duplicate variant with type Integer
    File: "":1:0
      |
    1 | ...
      | ^^^ Duplicated variant in this union
    Error[1]: Duplicate variant with type Integer
    File: "":11:14
       |
    11 |       let _ = Test2[Integer];
       |               ^^^^^^^^^^^^^ Duplicated variant in this union |}]

let%expect_test "type errors" =
  let source =
    {|
      struct Test {}

      fn test1() -> Test {
        123
      }

      fn test2() -> Test {
        return 123;
      }

      fn expect_test(t: Test) {}
      expect_test(123);
    |}
  in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Expected type `<struct 1>` but found `Integer`
    File: "":5:8
      |
    5 |         123
      |         ^^^ This has type `Integer`
    Error[1]: Expected type `<struct 1>` but found `Integer`
    File: "":9:15
      |
    9 |         return 123;
      |                ^^^ This has type `Integer`
    Error[1]: Expected type `<struct 1>` but found `Integer`
    File: "":13:18
       |
    13 |       expect_test(123);
       |                   ^^^ This has type `Integer`
    Error[1]: Expected 1 arguments but found 1.
    File: "":13:6
       |
    13 |       expect_test(123);
       |       ^^^^^^^^^^^ When calling this function |}]

let%expect_test "is not a struct error" =
  let source =
    {|
      fn test() { 123 }
      let a = test() { field: 123 };
    |}
  in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Expression is not struct type, so it cannot be used in such context.
    File: "":3:14
      |
    3 |       let a = test() { field: 123 };
      |               ^^^^^^ This is not struct type |}]

let%expect_test "cannot have methods error" =
  let source = {|
      123.test();
    |} in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Type `Integer` cannot have methods.
    File: "":2:6
      |
    2 |       123.test();
      |       ^^^ This cannot have methods |}]

let%expect_test "this cannot be called error" =
  let source = {|
      123();
    |} in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Expected function but got value with `Integer` type.
    File: "":2:6
      |
    2 |       123();
      |       ^^^ This cannot be called |}]

let%expect_test "argument number mismatch" =
  let source = {|
      fn test(x: Integer) {}
      test(10, 20, 30);
    |} in
  pp source ~include_std:false ;
  [%expect
    {|
        Error[1]: Expected 1 arguments but found 3.
        File: "":3:6
          |
        3 |       test(10, 20, 30);
          |       ^^^^ When calling this function |}]

(* FIXME: this should print error. *)
let%expect_test "uninterpretable statement" =
  let source =
    {|
      fn test() { builtin_begin_cell(); }
      test();
    |}
  in
  pp source ~include_std:false ;
  [%expect
    {|
        (Ok
         ((bindings
           ((((span (pos pos)) (value test))
             ((span (pos pos))
              (value
               (Value
                (Function
                 ((span (pos pos))
                  (value
                   ((function_signature
                     ((span (pos pos))
                      (value ((function_params ()) (function_returns HoleType)))))
                    (function_impl
                     (Fn
                      ((span (pos pos))
                       (value
                        (Return
                         ((span (pos pos))
                          (value
                           (FunctionCall
                            (((span (pos pos))
                              (value
                               (ResolvedReference
                                (((span (pos pos)) (value builtin_begin_cell))
                                 <opaque>))))
                             () false)))))))))))))))))))
          (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
          (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "field not found" =
  let source =
    {|
      struct Empty {}
      let _ = Empty{}.field;
      let {field} = Empty{};
    |}
  in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Field `field` not found.
    File: "":3:22
      |
    3 |       let _ = Empty{}.field;
      |                       ^^^^^ This field not found
    Error[1]: Field `field` not found.
    File: "":4:11
      |
    4 |       let {field} = Empty{};
      |            ^^^^^ This field not found |}]

let%expect_test "missing field error" =
  let source =
    {|
      struct Test { val field: Integer }
      let {} = Test{field: 10};
    |}
  in
  pp source ~include_std:false ;
  [%expect
    {|
    Error[1]: Field `field` missing in destructuring statement.
    File: "":3:6
      |
    3 |       let {} = Test{field: 10};
      |       ^^^^^^^^^^^^^^^^^^^^^^^^ In this binding |}]

let%expect_test "Case Not Found Error" =
  let source =
    {|
      union Test { 
        case Int[10] 
        case Int[20]
      }
      fn test(value: Test) {
        switch (value) {
          case Int[10] _ => { return 10; }
          case Int[123] _ => { return 123; }
        }
      }
    |}
  in
  pp source ;
  [%expect
    {|
    Error[1]: Case type not found in union.
    File: "":9:24
      |
    9 |           case Int[123] _ => { return 123; }
      |                         ^ Type of this variable is not found in the condition union |}]

let%expect_test "Expected Type Function" =
  let source =
    {|
      union Test[X: Type] {}

      let _ = Test(Integer);
    |}
  in
  pp source ;
  [%expect
    {|
    Error[1]: Function should be called using `[]` brackets but called with `()` parens.
    File: "":4:14
      |
    4 |       let _ = Test(Integer);
      |               ^^^^^^^^^^^^ When calling this function |}]
