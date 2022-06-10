open Shared

let%expect_test "Int(bits) constructor" =
  let source =
    {|
      let i = Int(257).new(100);
      let overflow = Int(8).new(513);
    |}
  in
  pp source ;
  [%expect{|
    (Ok
     ((bindings
       ((overflow (Value (Struct (0 ((integer (Value (Integer 513))))))))
        (i (Value (Struct (0 ((integer (Value (Integer 100))))))))))
      (structs
       ((0
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 0))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 0)) (b (BuiltinType Builder))))
                (function_returns (BuiltinType Builder))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt (builder (Reference (b (BuiltinType Builder))))
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
  pp source ;
  [%expect{|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((b (BuiltinType Builder))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((i (Value (Struct (0 ((integer (Value (Integer 100))))))))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (serialize <opaque>))
                    ((ResolvedReference (i <opaque>))
                     (Reference (b (BuiltinType Builder))))))))))))))))))
      (structs
       ((0
         ((struct_fields ((integer ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((integer IntegerType)))
                (function_returns (StructType 0))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 0)) (b (BuiltinType Builder))))
                (function_returns (BuiltinType Builder))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt (builder (Reference (b (BuiltinType Builder))))
                    (length (Value (Integer 32)))
                    (integer
                     (StructField ((Reference (self (StructType 0))) integer)))
                    (signed true)))))))))))
          (struct_impls ()) (struct_id 0)))))
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
  [%expect{|
    (Error
     (((UnexpectedType (TypeN 0))
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ()) (function_returns HoleType)))
              (function_impl
               (Fn ((Block ((Let ((b (Value Void)))) (Expr (Value Void)))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((self (StructType 1)) (b (BuiltinType Builder))))
                (function_returns (BuiltinType Builder))))
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
                              ((self (StructType 0)) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 32)))
                                 (integer
                                  (StructField
                                   ((Reference (self (StructType 0))) integer)))
                                 (signed true))))))))))
                        ((StructField ((Reference (self (StructType 1))) a))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 0)) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 32)))
                                 (integer
                                  (StructField
                                   ((Reference (self (StructType 0))) integer)))
                                 (signed true))))))))))
                        ((StructField ((Reference (self (StructType 1))) b))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Return (Reference (b (BuiltinType Builder)))))))))))))
          (T (Value (Type (StructType 1))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (structs
         ((1
           ((struct_fields
             ((a ((field_type (StructType 0))))
              (b ((field_type (StructType 0))))))
            (struct_methods ()) (struct_impls ()) (struct_id 1)))
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
                 ((function_params
                   ((self (StructType 0)) (b (BuiltinType Builder))))
                  (function_returns (BuiltinType Builder))))
                (function_impl
                 (Fn
                  ((Return
                    (Primitive
                     (StoreInt (builder (Reference (b (BuiltinType Builder))))
                      (length (Value (Integer 32)))
                      (integer
                       (StructField ((Reference (self (StructType 0))) integer)))
                      (signed true)))))))))))
            (struct_impls ()) (struct_id 0)))))
        (struct_counter <opaque>) (memoized_fcalls <opaque>)))
      ((TypeError ((BuiltinType Builder) VoidType))
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ()) (function_returns HoleType)))
              (function_impl
               (Fn ((Block ((Let ((b (Value Void)))) (Expr (Value Void)))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((self (StructType 1)) (b (BuiltinType Builder))))
                (function_returns (BuiltinType Builder))))
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
                              ((self (StructType 0)) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 32)))
                                 (integer
                                  (StructField
                                   ((Reference (self (StructType 0))) integer)))
                                 (signed true))))))))))
                        ((StructField ((Reference (self (StructType 1))) a))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 0)) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 32)))
                                 (integer
                                  (StructField
                                   ((Reference (self (StructType 0))) integer)))
                                 (signed true))))))))))
                        ((StructField ((Reference (self (StructType 1))) b))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Return (Reference (b (BuiltinType Builder)))))))))))))
          (T (Value (Type (StructType 1))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (structs
         ((1
           ((struct_fields
             ((a ((field_type (StructType 0))))
              (b ((field_type (StructType 0))))))
            (struct_methods ()) (struct_impls ()) (struct_id 1)))
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
                 ((function_params
                   ((self (StructType 0)) (b (BuiltinType Builder))))
                  (function_returns (BuiltinType Builder))))
                (function_impl
                 (Fn
                  ((Return
                    (Primitive
                     (StoreInt (builder (Reference (b (BuiltinType Builder))))
                      (length (Value (Integer 32)))
                      (integer
                       (StructField ((Reference (self (StructType 0))) integer)))
                      (signed true)))))))))))
            (struct_impls ()) (struct_id 0)))))
        (struct_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

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
