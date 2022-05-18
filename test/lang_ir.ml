open Shared
open Tact.Lang_types

let rec print_sexp' e =
  pp_sexp
    (Result.sexp_of_t Tact.Lang_ir.sexp_of_program
       (List.sexp_of_t sexp_of_error)
       e )

and pp ?(bindings = Lang.default_bindings) s =
  let p = parse_program s |> build_program ~bindings in
  print_sexp p ;
  let p' =
    Result.map p ~f:(fun p ->
        let c = new Tact.Lang_ir.constructor (p, new Errors.errors) in
        c#visit_program () p )
  in
  print_sexp' p'

let incr_f =
  Function
    { function_params = [("value", Value (Type IntegerType))];
      function_returns = Value (Type IntegerType);
      function_impl =
        BuiltinFn
          (builtin_fun (fun _p -> function
             | Integer arg :: _ ->
                 Value (Integer (Zint.succ arg))
             | _ ->
                 Value (Integer Zint.zero) ) ) }

let%expect_test "scope resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 36, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((T
         (Value
          (Struct
           ((struct_fields
             ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_methods
             ((new
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
            (struct_id <opaque>)))))))))
    (bindings
     ((T
       (Value
        (Struct
         ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
          (struct_methods
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
          (struct_id <opaque>))))))))) |}]

let%expect_test "binding resolution" =
  let source =
    {|
    let T = Int(257);
    let T_ = T;
    let a = 1;
    let a_ = a;
  |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 94, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((T
         (Value
          (Struct
           ((struct_fields
             ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_methods
             ((new
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
            (struct_id <opaque>)))))))
      (Let
       ((T_
         (Value
          (Struct
           ((struct_fields
             ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_methods
             ((new
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
            (struct_id <opaque>)))))))
      (Let ((a (Value (Integer 1))))) (Let ((a_ (Value (Integer 1)))))))
    (bindings
     ((a_ (Value (Integer 1))) (a (Value (Integer 1)))
      (T_
       (Value
        (Struct
         ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
          (struct_methods
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
          (struct_id <opaque>)))))
      (T
       (Value
        (Struct
         ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
          (struct_methods
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
          (struct_id <opaque>))))))))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error ((UnresolvedIdentifier Int256)))(Error
                                            ((UnresolvedIdentifier Int256))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 180, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((A
         (Value
          (Struct
           ((struct_fields
             ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_methods
             ((new
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
            (struct_id <opaque>)))))))
      (Let
       ((B
         (Value
          (Struct
           ((struct_fields
             ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_methods
             ((new
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
            (struct_id <opaque>)))))))))
    (bindings
     ((B
       (Value
        (Struct
         ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
          (struct_methods
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
          (struct_id <opaque>)))))
      (A
       (Value
        (Struct
         ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
          (struct_methods
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
          (struct_id <opaque>))))))))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_struct_field in file "lib/lang_ir.ml", line 189, characters 25-63
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 253, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((T
         (Value
          (Struct
           ((struct_fields
             ((t
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_methods
                     ((new
                       ((function_params
                         ((integer (Value (Type IntegerType)))))
                        (function_returns Hole)
                        (function_impl (BuiltinFn (<fun> 1)))))))
                    (struct_id <opaque>)))))))))
            (struct_methods ()) (struct_id <opaque>)))))))))
    (bindings
     ((T
       (Value
        (Struct
         ((struct_fields
           ((t
             ((field_type
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))))
          (struct_methods ()) (struct_id <opaque>))))))))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
  [%expect
    {|
    (Ok
     ((stmts ((Let ((v (Value (Integer 4)))))))
      (bindings ((v (Value (Integer 4)))))))(Ok
                                             ((bindings
                                               ((v (Value (Integer 4))))))) |}]

let%expect_test "Tact function evaluation" =
  let source =
    {|
    fn test(i: Int(257)) -> Int(257) {
      i
    }
    let a = test(test(1));
  |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_.(fun) in file "lib/lang_ir.ml", line 173, characters 31-68
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 170, characters 8-284
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 340, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((test
         (Value
          (Function
           ((function_params
             ((i
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))
            (function_returns
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))
            (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
      (Let ((a (Value (Integer 1)))))))
    (bindings
     ((a (Value (Integer 1)))
      (test
       (Value
        (Function
         ((function_params
           ((i
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))))
          (function_returns
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_methods
               ((new
                 ((function_params ((integer (Value (Type IntegerType)))))
                  (function_returns Hole)
                  (function_impl (BuiltinFn (<fun> 1)))))))
              (struct_id <opaque>)))))
          (function_impl (Fn (((Break (Expr (Reference (i TypeType)))))))))))))))) |}]

let%expect_test "compile-time function evaluation within a function" =
  let source =
    {|
    fn test() {
      let v = incr(incr(incr(1)));
      v
    }
  |}
  in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 435, characters 2-71
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((test
         (Value
          (Function
           ((function_params ()) (function_returns Hole)
            (function_impl
             (Fn
              (((Let ((v (Value (Integer 4)))))
                (Break (Expr (Value (Integer 4))))))))))))))))
    (bindings
     ((test
       (Value
        (Function
         ((function_params ()) (function_returns Hole)
          (function_impl
           (Fn
            (((Let ((v (Value (Integer 4)))))
              (Break (Expr (Value (Integer 4)))))))))))))))) |}]

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
       val a: Int(257)
       val b: Bool
  };
  |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_struct_field in file "lib/lang_ir.ml", line 189, characters 25-63
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 487, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((MyType
         (Value
          (Struct
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_methods
                     ((new
                       ((function_params
                         ((integer (Value (Type IntegerType)))))
                        (function_returns Hole)
                        (function_impl (BuiltinFn (<fun> 1)))))))
                    (struct_id <opaque>)))))))
              (b ((field_type (Value (Builtin Bool)))))))
            (struct_methods ()) (struct_id <opaque>)))))))))
    (bindings
     ((MyType
       (Value
        (Struct
         ((struct_fields
           ((a
             ((field_type
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))
            (b ((field_type (Value (Builtin Bool)))))))
          (struct_methods ()) (struct_id <opaque>))))))))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      val a: Int(257)
      val a: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
    (Error
     ((DuplicateField
       (a
        ((struct_fields
          ((a
            ((field_type
              (Value
               (Struct
                ((struct_fields
                  ((integer ((field_type (Value (Type IntegerType)))))))
                 (struct_methods
                  ((new
                    ((function_params ((integer (Value (Type IntegerType)))))
                     (function_returns Hole)
                     (function_impl (BuiltinFn (<fun> 1)))))))
                 (struct_id <opaque>)))))))
           (a ((field_type (Value (Builtin Bool)))))))
         (struct_methods ()) (struct_id <opaque>))))))(Error
                                                       ((DuplicateField
                                                         (a
                                                          ((struct_fields
                                                            ((a
                                                              ((field_type
                                                                (Value
                                                                 (Struct
                                                                  ((struct_fields
                                                                    ((integer
                                                                      ((field_type
                                                                        (Value
                                                                        (Type
                                                                        IntegerType)))))))
                                                                   (struct_methods
                                                                    ((new
                                                                      ((function_params
                                                                        ((integer
                                                                        (Value
                                                                        (Type
                                                                        IntegerType)))))
                                                                       (function_returns
                                                                        Hole)
                                                                       (function_impl
                                                                        (BuiltinFn
                                                                        (<fun> 1)))))))
                                                                   (struct_id
                                                                    <opaque>)))))))
                                                             (a
                                                              ((field_type
                                                                (Value
                                                                 (Builtin Bool)))))))
                                                           (struct_methods ())
                                                           (struct_id <opaque>)))))) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T(A: Type) { val a: A }
      let TA = T(Int(257));
   |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_struct_field in file "lib/lang_ir.ml", line 189, characters 25-63
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Struct in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 625, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((T
         (Value
          (Function
           ((function_params ((A (Value (Builtin Type)))))
            (function_returns (Value (Builtin Type)))
            (function_impl
             (Fn
              (((Expr
                 (Value
                  (Struct
                   ((struct_fields
                     ((a ((field_type (Reference (A (BuiltinType Type))))))))
                    (struct_methods ()) (struct_id <opaque>)))))))))))))))
      (Let
       ((TA
         (Value
          (Struct
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_methods
                     ((new
                       ((function_params
                         ((integer (Value (Type IntegerType)))))
                        (function_returns Hole)
                        (function_impl (BuiltinFn (<fun> 1)))))))
                    (struct_id <opaque>)))))))))
            (struct_methods ()) (struct_id <opaque>)))))))))
    (bindings
     ((TA
       (Value
        (Struct
         ((struct_fields
           ((a
             ((field_type
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))))
          (struct_methods ()) (struct_id <opaque>)))))
      (T
       (Value
        (Function
         ((function_params ((A (Value (Builtin Type)))))
          (function_returns (Value (Builtin Type)))
          (function_impl
           (Fn
            (((Expr
               (Value
                (Struct
                 ((struct_fields
                   ((a ((field_type (Reference (A (BuiltinType Type))))))))
                  (struct_methods ()) (struct_id <opaque>))))))))))))))))) |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 722, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((f
         (Value
          (Function
           ((function_params ()) (function_returns Hole)
            (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))))
      (Let ((a (Value (Integer 1)))))))
    (bindings
     ((a (Value (Integer 1)))
      (f
       (Value
        (Function
         ((function_params ()) (function_returns Hole)
          (function_impl (Fn (((Break (Expr (Value (Integer 1)))))))))))))))) |}]

let%expect_test "scoping that `let` introduces in code" =
  let source =
    {|
    fn f(i: Int(257)) {
      let a = i;
      a
    }
    let b = f(1);
    |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_.(fun) in file "lib/lang_ir.ml", line 173, characters 31-68
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 170, characters 8-284
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 772, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((f
         (Value
          (Function
           ((function_params
             ((i
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))
            (function_returns Hole)
            (function_impl
             (Fn
              (((Let ((a (Reference (i TypeType)))))
                (Break (Expr (Reference (a TypeType))))))))))))))
      (Let ((b (Value (Integer 1)))))))
    (bindings
     ((b (Value (Integer 1)))
      (f
       (Value
        (Function
         ((function_params
           ((i
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))))
          (function_returns Hole)
          (function_impl
           (Fn
            (((Let ((a (Reference (i TypeType)))))
              (Break (Expr (Reference (a TypeType)))))))))))))))) |}]

let%expect_test "reference in function bodies" =
  let source =
    {|
      fn op(i: Int(257), i_: Int(257)) {
        i
      }

      fn f(x: Int(257)) {
        let a = op(x, x);
        let b = op(a, a);
      }
    |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_.(fun) in file "lib/lang_ir.ml", line 173, characters 31-68
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 170, characters 8-284
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 857, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((op
         (Value
          (Function
           ((function_params
             ((i
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))
              (i_
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))
            (function_returns Hole)
            (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
      (Let
       ((f
         (Value
          (Function
           ((function_params
             ((x
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_methods
                   ((new
                     ((function_params ((integer (Value (Type IntegerType)))))
                      (function_returns Hole)
                      (function_impl (BuiltinFn (<fun> 1)))))))
                  (struct_id <opaque>)))))))
            (function_returns Hole)
            (function_impl
             (Fn
              (((Let
                 ((a
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_params
                         ((i
                           (Value
                            (Struct
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_methods
                               ((new
                                 ((function_params
                                   ((integer (Value (Type IntegerType)))))
                                  (function_returns Hole)
                                  (function_impl (BuiltinFn (<fun> 1)))))))
                              (struct_id <opaque>)))))
                          (i_
                           (Value
                            (Struct
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_methods
                               ((new
                                 ((function_params
                                   ((integer (Value (Type IntegerType)))))
                                  (function_returns Hole)
                                  (function_impl (BuiltinFn (<fun> 1)))))))
                              (struct_id <opaque>)))))))
                        (function_returns Hole)
                        (function_impl
                         (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                     ((Reference (x TypeType)) (Reference (x TypeType))))))))
                (Let
                 ((b
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_params
                         ((i
                           (Value
                            (Struct
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_methods
                               ((new
                                 ((function_params
                                   ((integer (Value (Type IntegerType)))))
                                  (function_returns Hole)
                                  (function_impl (BuiltinFn (<fun> 1)))))))
                              (struct_id <opaque>)))))
                          (i_
                           (Value
                            (Struct
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_methods
                               ((new
                                 ((function_params
                                   ((integer (Value (Type IntegerType)))))
                                  (function_returns Hole)
                                  (function_impl (BuiltinFn (<fun> 1)))))))
                              (struct_id <opaque>)))))))
                        (function_returns Hole)
                        (function_impl
                         (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                     ((Reference (a HoleType)) (Reference (a HoleType))))))))))))))))))))
    (bindings
     ((f
       (Value
        (Function
         ((function_params
           ((x
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))))
          (function_returns Hole)
          (function_impl
           (Fn
            (((Let
               ((a
                 (FunctionCall
                  ((Value
                    (Function
                     ((function_params
                       ((i
                         (Value
                          (Struct
                           ((struct_fields
                             ((integer
                               ((field_type (Value (Type IntegerType)))))))
                            (struct_methods
                             ((new
                               ((function_params
                                 ((integer (Value (Type IntegerType)))))
                                (function_returns Hole)
                                (function_impl (BuiltinFn (<fun> 1)))))))
                            (struct_id <opaque>)))))
                        (i_
                         (Value
                          (Struct
                           ((struct_fields
                             ((integer
                               ((field_type (Value (Type IntegerType)))))))
                            (struct_methods
                             ((new
                               ((function_params
                                 ((integer (Value (Type IntegerType)))))
                                (function_returns Hole)
                                (function_impl (BuiltinFn (<fun> 1)))))))
                            (struct_id <opaque>)))))))
                      (function_returns Hole)
                      (function_impl
                       (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                   ((Reference (x TypeType)) (Reference (x TypeType))))))))
              (Let
               ((b
                 (FunctionCall
                  ((Value
                    (Function
                     ((function_params
                       ((i
                         (Value
                          (Struct
                           ((struct_fields
                             ((integer
                               ((field_type (Value (Type IntegerType)))))))
                            (struct_methods
                             ((new
                               ((function_params
                                 ((integer (Value (Type IntegerType)))))
                                (function_returns Hole)
                                (function_impl (BuiltinFn (<fun> 1)))))))
                            (struct_id <opaque>)))))
                        (i_
                         (Value
                          (Struct
                           ((struct_fields
                             ((integer
                               ((field_type (Value (Type IntegerType)))))))
                            (struct_methods
                             ((new
                               ((function_params
                                 ((integer (Value (Type IntegerType)))))
                                (function_returns Hole)
                                (function_impl (BuiltinFn (<fun> 1)))))))
                            (struct_id <opaque>)))))))
                      (function_returns Hole)
                      (function_impl
                       (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                   ((Reference (a HoleType)) (Reference (a HoleType))))))))))))))))
      (op
       (Value
        (Function
         ((function_params
           ((i
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))
            (i_
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_methods
                 ((new
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole)
                    (function_impl (BuiltinFn (<fun> 1)))))))
                (struct_id <opaque>)))))))
          (function_returns Hole)
          (function_impl (Fn (((Break (Expr (Reference (i TypeType)))))))))))))))) |}]

let%expect_test "resolving a reference from inside a function" =
  let source =
    {|
      let i = 1;
      fn f() {
        i
      }
      let x = f();
    |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 1133, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let ((i (Value (Integer 1)))))
      (Let
       ((f
         (Value
          (Function
           ((function_params ()) (function_returns Hole)
            (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))))
      (Let ((x (Value (Integer 1)))))))
    (bindings
     ((x (Value (Integer 1)))
      (f
       (Value
        (Function
         ((function_params ()) (function_returns Hole)
          (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))
      (i (Value (Integer 1))))))) |}]

let%expect_test "method access" =
  let source =
    {|
      struct Foo {
        fn bar(self: Type, i: Integer) {
           i
        }
      }
      let foo = Foo {};
      let res = foo.bar(1);
    |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_.(fun) in file "lib/lang_ir.ml", line 173, characters 31-68
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 170, characters 8-284
  Called from Tact__Lang_types.visitor#visit_struct_.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_types.visitor#visit_struct_ in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_StructInstance.(fun) in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_StructInstance in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 265, characters 15-41
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 1187, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((Foo
         (Value
          (Struct
           ((struct_fields ())
            (struct_methods
             ((bar
               ((function_params
                 ((self (Value (Builtin Type))) (i (Value (Type IntegerType)))))
                (function_returns Hole)
                (function_impl
                 (Fn (((Break (Expr (Reference (i IntegerType))))))))))))
            (struct_id <opaque>)))))))
      (Let
       ((foo
         (Value
          (StructInstance
           (((struct_fields ())
             (struct_methods
              ((bar
                ((function_params
                  ((self (Value (Builtin Type)))
                   (i (Value (Type IntegerType)))))
                 (function_returns Hole)
                 (function_impl
                  (Fn (((Break (Expr (Reference (i IntegerType))))))))))))
             (struct_id <opaque>))
            ()))))))
      (Let ((res (Value (Integer 1)))))))
    (bindings
     ((res (Value (Integer 1)))
      (foo
       (Value
        (StructInstance
         (((struct_fields ())
           (struct_methods
            ((bar
              ((function_params
                ((self (Value (Builtin Type))) (i (Value (Type IntegerType)))))
               (function_returns Hole)
               (function_impl
                (Fn (((Break (Expr (Reference (i IntegerType))))))))))))
           (struct_id <opaque>))
          ()))))
      (Foo
       (Value
        (Struct
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_params
               ((self (Value (Builtin Type))) (i (Value (Type IntegerType)))))
              (function_returns Hole)
              (function_impl
               (Fn (((Break (Expr (Reference (i IntegerType))))))))))))
          (struct_id <opaque>))))))))) |}]

let%expect_test "can't use asm in compile-time" =
  let source = {|
    asm("SWAP 2 XCHG0");
    asm("ADD");
  |} in
  pp source ;
  [%expect
    {|
    (Error
     ((UninterpretableStatement (Expr (Asm (SWAP (XCHG0 2)))))
      (UninterpretableStatement (Expr (Asm (ADD))))))(Error
                                                      ((UninterpretableStatement
                                                        (Expr
                                                         (Asm (SWAP (XCHG0 2)))))
                                                       (UninterpretableStatement
                                                        (Expr (Asm (ADD)))))) |}]

let%expect_test "use of asm in a function" =
  let source =
    {|
    fn f() {
      asm("SWAP 2 XCHG0");
      asm("ADD");
    }
  |}
  in
  pp source ; [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Tact.Lang_ir.Invalid)
  Raised at Tact__Lang_ir.constructor#visit_expr_as_type in file "lib/lang_ir.ml", line 160, characters 64-77
  Called from Tact__Lang_ir.constructor#visit_function_ in file "lib/lang_ir.ml", line 177, characters 25-68
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_Value in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from Tact__Lang_types.visitor#visit_binding in file "lib/lang_types.ml", line 19, characters 0-1023
  Called from VisitorsRuntime.map#visit_list in file "runtime/VisitorsRuntime.ml", line 264, characters 18-25
  Called from Tact__Lang_ir.constructor#visit_program in file "lib/lang_ir.ml", line 193, characters 18-61
  Called from Base__Result.map in file "src/result.ml", line 157, characters 19-24
  Called from Tact_tests__Lang_ir.pp in file "test/lang_ir.ml", line 14, characters 4-131
  Called from Tact_tests__Lang_ir.(fun) in file "test/lang_ir.ml", line 1301, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  (Ok
   ((stmts
     ((Let
       ((f
         (Value
          (Function
           ((function_params ()) (function_returns Hole)
            (function_impl
             (Fn (((Expr (Asm (SWAP (XCHG0 2)))) (Expr (Asm (ADD)))))))))))))))
    (bindings
     ((f
       (Value
        (Function
         ((function_params ()) (function_returns Hole)
          (function_impl
           (Fn (((Expr (Asm (SWAP (XCHG0 2)))) (Expr (Asm (ADD))))))))))))))) |}]
