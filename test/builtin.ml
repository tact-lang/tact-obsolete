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
          (StructInstance
           (((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))
            ((integer (Value (Integer 1))))))))
        (i
         (Value
          (StructInstance
           (((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))
            ((integer (Value (Integer 100))))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (b (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder (Reference (b (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 8)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true)))))))))))
        ((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (b (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder (Reference (b (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
               ((function_params ((b (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type HoleType)))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((i
                      (Value
                       (StructInstance
                        (((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>))
                         ((integer (Value (Integer 100))))))))))
                   (Expr
                    (FunctionCall
                     ((ResolvedReference (serialize <opaque>))
                      ((ResolvedReference (i <opaque>))
                       (Reference (b (Value (Type (BuiltinType Builder))))))))))))))))))))
        (methods
         (((Type
            (StructType
             ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>))))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))
                  (b (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder (Reference (b (Value (Type (BuiltinType Builder))))))
                    (length (Value (Integer 32)))
                    (integer
                     (StructField
                      ((Reference
                        (self
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       integer)))
                    (signed true))))))))))))))) |}]

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
             ((function_params ()) (function_returns (Value (Type HoleType)))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((b (FunctionCall ((ResolvedReference (new <opaque>)) ())))))
                 (Expr
                  (FunctionCall
                   ((ResolvedReference (T_serializer <opaque>))
                    ((Value
                      (StructInstance
                       (((struct_fields
                          ((a
                            ((field_type
                              (Value
                               (Type
                                (StructType
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>))))))))
                           (b
                            ((field_type
                              (Value
                               (Type
                                (StructType
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>))))))))))
                         (struct_id <opaque>))
                        ((a
                          (Value
                           (StructInstance
                            (((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_id <opaque>))
                             ((integer (Value (Integer 0))))))))
                         (b
                          (Value
                           (StructInstance
                            (((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_id <opaque>))
                             ((integer (Value (Integer 1))))))))))))
                     (Reference (b (Value (Type (BuiltinType Builder))))))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((a
                        ((field_type
                          (Value
                           (Type
                            (StructType
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_id <opaque>))))))))
                       (b
                        ((field_type
                          (Value
                           (Type
                            (StructType
                             ((struct_fields
                               ((integer
                                 ((field_type (Value (Type IntegerType)))))))
                              (struct_id <opaque>))))))))))
                     (struct_id <opaque>))))))
                (b (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
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
                            ((self
                              (Value
                               (Type
                                (StructType
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>))))))
                             (b (Value (Type (BuiltinType Builder))))))
                           (function_returns
                            (Value (Type (BuiltinType Builder))))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (Reference
                                 (b (Value (Type (BuiltinType Builder))))))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference
                                   (self
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>)))))))
                                  integer)))
                               (signed true))))))))))
                      ((StructField
                        ((Reference
                          (self
                           (Value
                            (Type
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>))))))))
                                 (b
                                  ((field_type
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>))))))))))
                               (struct_id <opaque>)))))))
                         a))
                       (Reference (b (Value (Type (BuiltinType Builder)))))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self
                              (Value
                               (Type
                                (StructType
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>))))))
                             (b (Value (Type (BuiltinType Builder))))))
                           (function_returns
                            (Value (Type (BuiltinType Builder))))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (Reference
                                 (b (Value (Type (BuiltinType Builder))))))
                               (length (Value (Integer 16)))
                               (integer
                                (StructField
                                 ((Reference
                                   (self
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>)))))))
                                  integer)))
                               (signed true))))))))))
                      ((StructField
                        ((Reference
                          (self
                           (Value
                            (Type
                             (StructType
                              ((struct_fields
                                ((a
                                  ((field_type
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>))))))))
                                 (b
                                  ((field_type
                                    (Value
                                     (Type
                                      (StructType
                                       ((struct_fields
                                         ((integer
                                           ((field_type
                                             (Value (Type IntegerType)))))))
                                        (struct_id <opaque>))))))))))
                               (struct_id <opaque>)))))))
                         b))
                       (Reference (b (Value (Type (BuiltinType Builder)))))))))))
                 (Return (Reference (b (Value (Type (BuiltinType Builder)))))))))))))))
        (T
         (Value
          (Type
           (StructType
            ((struct_fields
              ((a
                ((field_type
                  (Value
                   (Type
                    (StructType
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>))))))))
               (b
                ((field_type
                  (Value
                   (Type
                    (StructType
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>))))))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (b
               ((field_type
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))))
            (struct_id <opaque>))))
         ())
        ((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (b (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder (Reference (b (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 16)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true)))))))))))
        ((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (b (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder (Reference (b (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 32)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true)))))))))))))
      (impls
       (((Type
          (StructType
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (b
               ((field_type
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))))
            (struct_id <opaque>))))
         ()))))) |}]
