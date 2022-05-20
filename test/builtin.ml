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
            ((integer (Integer 1)))))))
        (i
         (Value
          (StructInstance
           (((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))
            ((integer (Integer 100)))))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
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
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (Primitive
                  (StoreInt (builder (Reference (builder (BuiltinType Builder))))
                   (length (Value (Integer 8)))
                   (integer
                    (StructField
                     ((Reference
                       (self
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>)))))
                      integer)))
                   (signed true))))))))))))
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
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
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (Primitive
                  (StoreInt (builder (Reference (builder (BuiltinType Builder))))
                   (length (Value (Integer 257)))
                   (integer
                    (StructField
                     ((Reference
                       (self
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>)))))
                      integer)))
                   (signed true))))))))))))
        ((Type (BuiltinType Builder))
         ((new
           ((function_signature
             ((function_params ())
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl (Fn (((Return (Primitive EmptyBuilder)))))))))))))) |}]

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
                (function_returns Hole)))
              (function_impl
               (Fn
                (((Let
                   ((i
                     (Value
                      (StructInstance
                       (((struct_fields
                          ((integer ((field_type (Value (Type IntegerType)))))))
                         (struct_id <opaque>))
                        ((integer (Integer 100)))))))))
                  (Expr
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((ResolvedReference (i <opaque>))
                      (Reference (b (BuiltinType Builder)))))))))))))))))
        (methods
         (((Struct
            ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
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
                  (builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                (((Return
                   (Primitive
                    (StoreInt (builder (Reference (builder (BuiltinType Builder))))
                     (length (Value (Integer 32)))
                     (integer
                      (StructField
                       ((Reference
                         (self
                          (StructType
                           ((struct_fields
                             ((integer ((field_type (Value (Type IntegerType)))))))
                            (struct_id <opaque>)))))
                        integer)))
                     (signed true))))))))))))
          ((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn (((Return (Primitive EmptyBuilder)))))))))))))) |}]

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
        T_serializer(T{}, b);
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
           ((function_signature ((function_params ()) (function_returns Hole)))
            (function_impl
             (Fn
              (((Let
                 ((b
                   (FunctionCall
                    ((ResolvedReference (new <opaque>))
                     ((ResolvedReference (Builder <opaque>))))))))
                (Expr
                 (FunctionCall
                  ((ResolvedReference (T_serializer <opaque>))
                   ((Value
                     (StructInstance
                      (((struct_fields
                         ((a
                           ((field_type
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))))
                          (b
                           ((field_type
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))))))
                        (struct_id <opaque>))
                       ())))
                    (Reference (b (BuiltinType Builder)))))))))))))))
        (T_serializer
         (Value
          (Function
           ((function_signature
             ((function_params
               ((self (Value (Type HoleType)))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type VoidType)))))
            (function_impl
             (Fn
              (((Let
                 ((builder
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
                            (builder (Value (Type (BuiltinType Builder))))))
                          (function_returns (Value (Type (BuiltinType Builder))))))
                        (function_impl
                         (Fn
                          (((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (Reference (builder (BuiltinType Builder))))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference
                                   (self
                                    (StructType
                                     ((struct_fields
                                       ((integer
                                         ((field_type (Value (Type IntegerType)))))))
                                      (struct_id <opaque>)))))
                                  integer)))
                               (signed true)))))))))))
                     ((StructField ((Reference (self HoleType)) a))
                      (Value (Type (BuiltinType Builder)))))))))
                (Let
                 ((builder
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
                            (builder (Value (Type (BuiltinType Builder))))))
                          (function_returns (Value (Type (BuiltinType Builder))))))
                        (function_impl
                         (Fn
                          (((Return
                             (Primitive
                              (StoreInt
                               (builder
                                (Reference (builder (BuiltinType Builder))))
                               (length (Value (Integer 16)))
                               (integer
                                (StructField
                                 ((Reference
                                   (self
                                    (StructType
                                     ((struct_fields
                                       ((integer
                                         ((field_type (Value (Type IntegerType)))))))
                                      (struct_id <opaque>)))))
                                  integer)))
                               (signed true)))))))))))
                     ((StructField ((Reference (self HoleType)) b))
                      (Value (Type (BuiltinType Builder)))))))))
                (Return (Reference (builder (BuiltinType Builder))))))))))))
        (T
         (Value
          (Struct
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (b
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))))
            (struct_id <opaque>)))))))
      (methods
       (((Struct
          ((struct_fields
            ((a
              ((field_type
                (Value
                 (Struct
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>)))))))
             (b
              ((field_type
                (Value
                 (Struct
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>)))))))))
           (struct_id <opaque>)))
         ())
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
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
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (Primitive
                  (StoreInt (builder (Reference (builder (BuiltinType Builder))))
                   (length (Value (Integer 16)))
                   (integer
                    (StructField
                     ((Reference
                       (self
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>)))))
                      integer)))
                   (signed true))))))))))))
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
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
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (Primitive
                  (StoreInt (builder (Reference (builder (BuiltinType Builder))))
                   (length (Value (Integer 32)))
                   (integer
                    (StructField
                     ((Reference
                       (self
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>)))))
                      integer)))
                   (signed true))))))))))))
        ((Type (BuiltinType Builder))
         ((new
           ((function_signature
             ((function_params ())
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl (Fn (((Return (Primitive EmptyBuilder)))))))))))))) |}]
