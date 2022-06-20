open Shared

let%expect_test "Int(bits) constructor" =
  let source =
    {|
      let i = Int(257).new(100);
      let overflow = Int(8).new(513);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((overflow (Value (Struct (1 ((integer (Value (Integer 1))))))))
        (i (Value (Struct (0 ((integer (Value (Integer 100))))))))))
      (structs
       ((1
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 1))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params ((self (StructType 1)) (b (StructType -1))))
                (function_returns (StructType -1))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (StructField ((Reference (b (StructType -1))) builder)))
                    (length (Value (Integer 8)))
                    (integer
                     (StructField ((Reference (self (StructType 1))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 1)))
        (0
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 0))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params ((self (StructType 0)) (b (StructType -1))))
                (function_returns (StructType -1))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (StructField ((Reference (b (StructType -1))) builder)))
                    (length (Value (Integer 257)))
                    (integer
                     (StructField ((Reference (self (StructType 0))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 0)))))
      (struct_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "Int(bits) serializer" =
  let source =
    {|
      fn test(b: Builder) {
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
  Called from Shared.build_program in file "test/shared.ml", line 24, characters 11-31
  Called from Shared.pp in file "test/shared.ml", line 52, characters 2-48
  Called from Tact_tests__Builtin.(fun) in file "test/builtin.ml", line 77, characters 2-11
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
  Called from Shared.build_program in file "test/shared.ml", line 24, characters 11-31
  Called from Shared.pp in file "test/shared.ml", line 52, characters 2-48
  Called from Tact_tests__Builtin.(fun) in file "test/builtin.ml", line 137, characters 2-11
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

let%expect_test "from interface" =
  let source =
    {|
      struct Value {
        val a: Integer
        impl From(Integer) {
          fn from(x: Integer) -> Self {
            Self{a: x}
          }
        }
      }
      fn check(y: Value) { y }

      let var = check(10);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((var (Value (Struct (0 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 1))))
              (function_returns (StructType 1))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (y (StructType 1))))))))))))))
        (Value (Value (Type (StructType 1))))))
      (structs
       ((1
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 1))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr (Value (Struct (0 ((a (Reference (x IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface
              (Value
               (Type
                (InterfaceType
                 ((interface_methods
                   ((from
                     ((function_params ((from IntegerType)))
                      (function_returns SelfType))))))))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns (StructType 1))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value (Struct (0 ((a (Reference (x IntegerType)))))))))))))))))))))))
          (struct_id 1)))))
      (struct_counter <opaque>) (memoized_fcalls <opaque>))) |}]
