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
       ((overflow
         (Value
          (Struct
           (((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 26)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 8)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 24))
            ((integer (Value (Integer 1))))))))
        (i
         (Value
          (Struct
           (((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0))
            ((integer (Value (Integer 100)))))))))))) |}]

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
               ((function_params ((b (BuiltinType Builder))))
                (function_returns HoleType)))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((i
                      (Value
                       (Struct
                        (((struct_fields ((integer ((field_type IntegerType)))))
                          (struct_methods
                           ((new
                             ((function_signature
                               ((function_params ((integer IntegerType)))
                                (function_returns SelfType)))
                              (function_impl (BuiltinFn (<fun> 8)))))
                            (serialize
                             ((function_signature
                               ((function_params
                                 ((self SelfType) (b (BuiltinType Builder))))
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
                                      ((Reference (self SelfType)) integer)))
                                    (signed true)))))))))))
                          (struct_impls ()) (struct_id 8))
                         ((integer (Value (Integer 100))))))))))
                   (Expr
                    (FunctionCall
                     ((ResolvedReference (serialize <opaque>))
                      ((ResolvedReference (i <opaque>))
                       (Reference (b (BuiltinType Builder)))))))))))))))))))) |}]

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
    (Error
     (((MethodNotFound ((Value (Type (BuiltinType Builder))) new))
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
                 ((self
                   (StructType
                    ((struct_fields
                      ((a
                        ((field_type
                          (StructType
                           ((struct_fields
                             ((integer ((field_type IntegerType)))))
                            (struct_methods
                             ((new
                               ((function_signature
                                 ((function_params ((integer IntegerType)))
                                  (function_returns SelfType)))
                                (function_impl (BuiltinFn (<fun> 8)))))
                              (serialize
                               ((function_signature
                                 ((function_params
                                   ((self SelfType) (b (BuiltinType Builder))))
                                  (function_returns (BuiltinType Builder))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Primitive
                                     (StoreInt
                                      (builder
                                       (Reference (b (BuiltinType Builder))))
                                      (length (Value (Integer 32)))
                                      (integer
                                       (StructField
                                        ((Reference (self SelfType)) integer)))
                                      (signed true)))))))))))
                            (struct_impls ()) (struct_id 8))))))
                       (b
                        ((field_type
                          (StructType
                           ((struct_fields
                             ((integer ((field_type IntegerType)))))
                            (struct_methods
                             ((new
                               ((function_signature
                                 ((function_params ((integer IntegerType)))
                                  (function_returns SelfType)))
                                (function_impl (BuiltinFn (<fun> 17)))))
                              (serialize
                               ((function_signature
                                 ((function_params
                                   ((self SelfType) (b (BuiltinType Builder))))
                                  (function_returns (BuiltinType Builder))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Primitive
                                     (StoreInt
                                      (builder
                                       (Reference (b (BuiltinType Builder))))
                                      (length (Value (Integer 16)))
                                      (integer
                                       (StructField
                                        ((Reference (self SelfType)) integer)))
                                      (signed true)))))))))))
                            (struct_impls ()) (struct_id 18))))))))
                     (struct_methods ()) (struct_impls ()) (struct_id 25))))
                  (b (BuiltinType Builder))))
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
                              ((self SelfType) (b (BuiltinType Builder))))
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
                                   ((Reference (self SelfType)) integer)))
                                 (signed true))))))))))
                        ((StructField
                          ((Reference
                            (self
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 8)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 32)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 8))))))
                                 (b
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 17)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 16)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 18))))))))
                               (struct_methods ()) (struct_impls ())
                               (struct_id 25)))))
                           a))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self SelfType) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 16)))
                                 (integer
                                  (StructField
                                   ((Reference (self SelfType)) integer)))
                                 (signed true))))))))))
                        ((StructField
                          ((Reference
                            (self
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 8)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 32)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 8))))))
                                 (b
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 17)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 16)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 18))))))))
                               (struct_methods ()) (struct_impls ())
                               (struct_id 25)))))
                           b))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Return (Reference (b (BuiltinType Builder)))))))))))))
          (T
           (Value
            (Type
             (StructType
              ((struct_fields
                ((a
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 8)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
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
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 8))))))
                 (b
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 17)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
                            (function_returns (BuiltinType Builder))))
                          (function_impl
                           (Fn
                            ((Return
                              (Primitive
                               (StoreInt
                                (builder (Reference (b (BuiltinType Builder))))
                                (length (Value (Integer 16)))
                                (integer
                                 (StructField
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 18))))))))
               (struct_methods ()) (struct_impls ()) (struct_id 25))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
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
              (function_impl (BuiltinFn (<fun> 1)))))))
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
              (function_impl (BuiltinFn (<fun> 2)))))))))))
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
                 ((self
                   (StructType
                    ((struct_fields
                      ((a
                        ((field_type
                          (StructType
                           ((struct_fields
                             ((integer ((field_type IntegerType)))))
                            (struct_methods
                             ((new
                               ((function_signature
                                 ((function_params ((integer IntegerType)))
                                  (function_returns SelfType)))
                                (function_impl (BuiltinFn (<fun> 8)))))
                              (serialize
                               ((function_signature
                                 ((function_params
                                   ((self SelfType) (b (BuiltinType Builder))))
                                  (function_returns (BuiltinType Builder))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Primitive
                                     (StoreInt
                                      (builder
                                       (Reference (b (BuiltinType Builder))))
                                      (length (Value (Integer 32)))
                                      (integer
                                       (StructField
                                        ((Reference (self SelfType)) integer)))
                                      (signed true)))))))))))
                            (struct_impls ()) (struct_id 8))))))
                       (b
                        ((field_type
                          (StructType
                           ((struct_fields
                             ((integer ((field_type IntegerType)))))
                            (struct_methods
                             ((new
                               ((function_signature
                                 ((function_params ((integer IntegerType)))
                                  (function_returns SelfType)))
                                (function_impl (BuiltinFn (<fun> 17)))))
                              (serialize
                               ((function_signature
                                 ((function_params
                                   ((self SelfType) (b (BuiltinType Builder))))
                                  (function_returns (BuiltinType Builder))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (Primitive
                                     (StoreInt
                                      (builder
                                       (Reference (b (BuiltinType Builder))))
                                      (length (Value (Integer 16)))
                                      (integer
                                       (StructField
                                        ((Reference (self SelfType)) integer)))
                                      (signed true)))))))))))
                            (struct_impls ()) (struct_id 18))))))))
                     (struct_methods ()) (struct_impls ()) (struct_id 25))))
                  (b (BuiltinType Builder))))
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
                              ((self SelfType) (b (BuiltinType Builder))))
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
                                   ((Reference (self SelfType)) integer)))
                                 (signed true))))))))))
                        ((StructField
                          ((Reference
                            (self
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 8)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 32)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 8))))))
                                 (b
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 17)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 16)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 18))))))))
                               (struct_methods ()) (struct_impls ())
                               (struct_id 25)))))
                           a))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Let
                    ((b
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self SelfType) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 16)))
                                 (integer
                                  (StructField
                                   ((Reference (self SelfType)) integer)))
                                 (signed true))))))))))
                        ((StructField
                          ((Reference
                            (self
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 8)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 32)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 8))))))
                                 (b
                                  ((field_type
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_methods
                                       ((new
                                         ((function_signature
                                           ((function_params
                                             ((integer IntegerType)))
                                            (function_returns SelfType)))
                                          (function_impl (BuiltinFn (<fun> 17)))))
                                        (serialize
                                         ((function_signature
                                           ((function_params
                                             ((self SelfType)
                                              (b (BuiltinType Builder))))
                                            (function_returns
                                             (BuiltinType Builder))))
                                          (function_impl
                                           (Fn
                                            ((Return
                                              (Primitive
                                               (StoreInt
                                                (builder
                                                 (Reference
                                                  (b (BuiltinType Builder))))
                                                (length (Value (Integer 16)))
                                                (integer
                                                 (StructField
                                                  ((Reference (self SelfType))
                                                   integer)))
                                                (signed true)))))))))))
                                      (struct_impls ()) (struct_id 18))))))))
                               (struct_methods ()) (struct_impls ())
                               (struct_id 25)))))
                           b))
                         (Reference (b (BuiltinType Builder)))))))))
                   (Return (Reference (b (BuiltinType Builder)))))))))))))
          (T
           (Value
            (Type
             (StructType
              ((struct_fields
                ((a
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 8)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
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
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 8))))))
                 (b
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 17)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
                            (function_returns (BuiltinType Builder))))
                          (function_impl
                           (Fn
                            ((Return
                              (Primitive
                               (StoreInt
                                (builder (Reference (b (BuiltinType Builder))))
                                (length (Value (Integer 16)))
                                (integer
                                 (StructField
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 18))))))))
               (struct_methods ()) (struct_impls ()) (struct_id 25))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
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
              (function_impl (BuiltinFn (<fun> 1)))))))
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
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

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
    (Error
     (((UnexpectedTypeSC SelfType)
       ((bindings
         ((var
           (Value
            (Struct
             (((struct_fields ()) (struct_methods ()) (struct_impls ())
               (struct_id 0))
              ()))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((y
                   (StructType
                    ((struct_fields ((a ((field_type IntegerType)))))
                     (struct_methods
                      ((from
                        ((function_signature
                          ((function_params ((x IntegerType)))
                           (function_returns SelfType)))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (Value
                                 (Struct
                                  (((struct_fields ()) (struct_methods ())
                                    (struct_impls ()) (struct_id 0))
                                   ()))))))))))))))
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
                                (function_returns SelfType)))
                              (function_impl
                               (Fn
                                ((Block
                                  ((Break
                                    (Expr
                                     (Value
                                      (Struct
                                       (((struct_fields ()) (struct_methods ())
                                         (struct_impls ()) (struct_id 0))
                                        ())))))))))))))))))))
                     (struct_id 26))))))
                (function_returns
                 (StructType
                  ((struct_fields ((a ((field_type IntegerType)))))
                   (struct_methods
                    ((from
                      ((function_signature
                        ((function_params ((x IntegerType)))
                         (function_returns SelfType)))
                       (function_impl
                        (Fn
                         ((Block
                           ((Break
                             (Expr
                              (Value
                               (Struct
                                (((struct_fields ()) (struct_methods ())
                                  (struct_impls ()) (struct_id 0))
                                 ()))))))))))))))
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
                              (function_returns SelfType)))
                            (function_impl
                             (Fn
                              ((Block
                                ((Break
                                  (Expr
                                   (Value
                                    (Struct
                                     (((struct_fields ()) (struct_methods ())
                                       (struct_impls ()) (struct_id 0))
                                      ())))))))))))))))))))
                   (struct_id 26))))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Reference
                      (y
                       (StructType
                        ((struct_fields ((a ((field_type IntegerType)))))
                         (struct_methods
                          ((from
                            ((function_signature
                              ((function_params ((x IntegerType)))
                               (function_returns SelfType)))
                             (function_impl
                              (Fn
                               ((Block
                                 ((Break
                                   (Expr
                                    (Value
                                     (Struct
                                      (((struct_fields ()) (struct_methods ())
                                        (struct_impls ()) (struct_id 0))
                                       ()))))))))))))))
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
                                    (function_returns SelfType)))
                                  (function_impl
                                   (Fn
                                    ((Block
                                      ((Break
                                        (Expr
                                         (Value
                                          (Struct
                                           (((struct_fields ())
                                             (struct_methods ())
                                             (struct_impls ()) (struct_id 0))
                                            ())))))))))))))))))))
                         (struct_id 26))))))))))))))))
          (Value
           (Value
            (Type
             (StructType
              ((struct_fields ((a ((field_type IntegerType)))))
               (struct_methods
                ((from
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns SelfType)))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct
                            (((struct_fields ()) (struct_methods ())
                              (struct_impls ()) (struct_id 0))
                             ()))))))))))))))
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
                          (function_returns SelfType)))
                        (function_impl
                         (Fn
                          ((Block
                            ((Break
                              (Expr
                               (Value
                                (Struct
                                 (((struct_fields ()) (struct_methods ())
                                   (struct_impls ()) (struct_id 0))
                                  ())))))))))))))))))))
               (struct_id 26))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
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
              (function_impl (BuiltinFn (<fun> 1)))))))
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
              (function_impl (BuiltinFn (<fun> 2)))))))))))
      ((TypeError
        (SelfType
         (StructType
          ((struct_fields ()) (struct_methods ()) (struct_impls ())
           (struct_id 0)))))
       ((bindings
         ((var
           (Value
            (Struct
             (((struct_fields ()) (struct_methods ()) (struct_impls ())
               (struct_id 0))
              ()))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((y
                   (StructType
                    ((struct_fields ((a ((field_type IntegerType)))))
                     (struct_methods
                      ((from
                        ((function_signature
                          ((function_params ((x IntegerType)))
                           (function_returns SelfType)))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (Value
                                 (Struct
                                  (((struct_fields ()) (struct_methods ())
                                    (struct_impls ()) (struct_id 0))
                                   ()))))))))))))))
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
                                (function_returns SelfType)))
                              (function_impl
                               (Fn
                                ((Block
                                  ((Break
                                    (Expr
                                     (Value
                                      (Struct
                                       (((struct_fields ()) (struct_methods ())
                                         (struct_impls ()) (struct_id 0))
                                        ())))))))))))))))))))
                     (struct_id 26))))))
                (function_returns
                 (StructType
                  ((struct_fields ((a ((field_type IntegerType)))))
                   (struct_methods
                    ((from
                      ((function_signature
                        ((function_params ((x IntegerType)))
                         (function_returns SelfType)))
                       (function_impl
                        (Fn
                         ((Block
                           ((Break
                             (Expr
                              (Value
                               (Struct
                                (((struct_fields ()) (struct_methods ())
                                  (struct_impls ()) (struct_id 0))
                                 ()))))))))))))))
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
                              (function_returns SelfType)))
                            (function_impl
                             (Fn
                              ((Block
                                ((Break
                                  (Expr
                                   (Value
                                    (Struct
                                     (((struct_fields ()) (struct_methods ())
                                       (struct_impls ()) (struct_id 0))
                                      ())))))))))))))))))))
                   (struct_id 26))))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Reference
                      (y
                       (StructType
                        ((struct_fields ((a ((field_type IntegerType)))))
                         (struct_methods
                          ((from
                            ((function_signature
                              ((function_params ((x IntegerType)))
                               (function_returns SelfType)))
                             (function_impl
                              (Fn
                               ((Block
                                 ((Break
                                   (Expr
                                    (Value
                                     (Struct
                                      (((struct_fields ()) (struct_methods ())
                                        (struct_impls ()) (struct_id 0))
                                       ()))))))))))))))
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
                                    (function_returns SelfType)))
                                  (function_impl
                                   (Fn
                                    ((Block
                                      ((Break
                                        (Expr
                                         (Value
                                          (Struct
                                           (((struct_fields ())
                                             (struct_methods ())
                                             (struct_impls ()) (struct_id 0))
                                            ())))))))))))))))))))
                         (struct_id 26))))))))))))))))
          (Value
           (Value
            (Type
             (StructType
              ((struct_fields ((a ((field_type IntegerType)))))
               (struct_methods
                ((from
                  ((function_signature
                    ((function_params ((x IntegerType)))
                     (function_returns SelfType)))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct
                            (((struct_fields ()) (struct_methods ())
                              (struct_impls ()) (struct_id 0))
                             ()))))))))))))))
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
                          (function_returns SelfType)))
                        (function_impl
                         (Fn
                          ((Block
                            ((Break
                              (Expr
                               (Value
                                (Struct
                                 (((struct_fields ()) (struct_methods ())
                                   (struct_impls ()) (struct_id 0))
                                  ())))))))))))))))))))
               (struct_id 26))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
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
              (function_impl (BuiltinFn (<fun> 1)))))))
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
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]
