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
       ((overflow (Value (Struct (100 ((integer (Value (Integer 513))))))))
        (i (Value (Struct (100 ((integer (Value (Integer 100))))))))))
      (structs
       ((100
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 100))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params ((self (StructType 100)) (b (StructType 0))))
                (function_returns (StructType 0))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (StructField ((Reference (b (StructType 0))) builder)))
                    (length (Value (Integer 257)))
                    (integer
                     (StructField ((Reference (self (StructType 100))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 100)))))
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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((b (StructType 0)))) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((i (Value (Struct (100 ((integer (Value (Integer 100))))))))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (serialize <opaque>))
                    ((ResolvedReference (i <opaque>))
                     (Reference (b (StructType 0))))))))))))))))))
      (structs
       ((100
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 100))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params ((self (StructType 100)) (b (StructType 0))))
                (function_returns (StructType 0))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (StructField ((Reference (b (StructType 0))) builder)))
                    (length (Value (Integer 32)))
                    (integer
                     (StructField ((Reference (self (StructType 100))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 100)))))
      (struct_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b (FunctionCall ((ResolvedReference (new <opaque>)) ())))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (T_serializer <opaque>))
                    ((Value
                      (Struct
                       (101
                        ((a
                          (Value (Struct (100 ((integer (Value (Integer 0))))))))
                         (b
                          (Value (Struct (100 ((integer (Value (Integer 1))))))))))))
                     (Reference (b (StructType 0))))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 101)) (b (StructType 0))))
              (function_returns (StructType 0))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 100)) (b (StructType 0))))
                           (function_returns (StructType 0))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (StructField
                                 ((Reference (b (StructType 0))) builder)))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference (self (StructType 100))) integer)))
                               (signed true))))))))))
                      ((StructField ((Reference (self (StructType 101))) a))
                       (Reference (b (StructType 0)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self (StructType 100)) (b (StructType 0))))
                           (function_returns (StructType 0))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (StructField
                                 ((Reference (b (StructType 0))) builder)))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference (self (StructType 100))) integer)))
                               (signed true))))))))))
                      ((StructField ((Reference (self (StructType 101))) b))
                       (Reference (b (StructType 0)))))))))
                 (Return (Reference (b (StructType 0)))))))))))))
        (T (Value (Type (StructType 101))))))
      (structs
       ((101
         ((struct_fields
           ((a ((field_type (StructType 100))))
            (b ((field_type (StructType 100))))))
          (struct_methods ()) (struct_impls ()) (struct_id 101)))
        (100
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 100))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params ((self (StructType 100)) (b (StructType 0))))
                (function_returns (StructType 0))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (StructField ((Reference (b (StructType 0))) builder)))
                    (length (Value (Integer 32)))
                    (integer
                     (StructField ((Reference (self (StructType 100))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 100)))))
      (struct_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
       ((var (Value (Struct (100 ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params ((y (StructType 101))))
              (function_returns (StructType 101))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (y (StructType 101))))))))))))))
        (Value (Value (Type (StructType 101))))))
      (structs
       ((101
         ((struct_fields ((a ((field_type IntegerType)))))
          (struct_methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns (StructType 101))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (100 ((a (Reference (x IntegerType))))))))))))))))))
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
                     (function_returns (StructType 101))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (100 ((a (Reference (x IntegerType)))))))))))))))))))))))
          (struct_id 101)))))
      (struct_counter <opaque>) (memoized_fcalls <opaque>))) |}]
