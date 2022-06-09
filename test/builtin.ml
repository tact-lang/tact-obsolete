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
             (struct_id <opaque>))
            ((integer (Value (Integer 1))))))))
        (i
         (Value
          (Struct
           (((struct_fields ((integer ((field_type IntegerType)))))
             (struct_id <opaque>))
            ((integer (Value (Integer 100))))))))))
      (infos ()) (def_infos ()))) |}]

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
                          (struct_id <opaque>))
                         ((integer (Value (Integer 100))))))))))
                   (Expr
                    (FunctionCall
                     ((ResolvedReference (serialize <opaque>))
                      ((ResolvedReference (i <opaque>))
                       (Reference (b (BuiltinType Builder))))))))))))))))))
        (infos ()) (def_infos ()))) |}]

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
                       (((struct_fields
                          ((a
                            ((field_type
                              (StructType
                               ((struct_fields
                                 ((integer ((field_type IntegerType)))))
                                (struct_id <opaque>))))))
                           (b
                            ((field_type
                              (StructType
                               ((struct_fields
                                 ((integer ((field_type IntegerType)))))
                                (struct_id <opaque>))))))))
                         (struct_id <opaque>))
                        ((a
                          (Value
                           (Struct
                            (((struct_fields
                               ((integer ((field_type IntegerType)))))
                              (struct_id <opaque>))
                             ((integer (Value (Integer 0))))))))
                         (b
                          (Value
                           (Struct
                            (((struct_fields
                               ((integer ((field_type IntegerType)))))
                              (struct_id <opaque>))
                             ((integer (Value (Integer 1))))))))))))
                     (Reference (b (BuiltinType Builder))))))))))))))))
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
                         ((struct_fields ((integer ((field_type IntegerType)))))
                          (struct_id <opaque>))))))
                     (b
                      ((field_type
                        (StructType
                         ((struct_fields ((integer ((field_type IntegerType)))))
                          (struct_id <opaque>))))))))
                   (struct_id <opaque>))))
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
                            ((self
                              (StructType
                               ((struct_fields
                                 ((integer ((field_type IntegerType)))))
                                (struct_id <opaque>))))
                             (b (BuiltinType Builder))))
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
                                 ((Reference
                                   (self
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_id <opaque>)))))
                                  integer)))
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
                                    (struct_id <opaque>))))))
                               (b
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((integer ((field_type IntegerType)))))
                                    (struct_id <opaque>))))))))
                             (struct_id <opaque>)))))
                         a))
                       (Reference (b (BuiltinType Builder)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self
                              (StructType
                               ((struct_fields
                                 ((integer ((field_type IntegerType)))))
                                (struct_id <opaque>))))
                             (b (BuiltinType Builder))))
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
                                 ((Reference
                                   (self
                                    (StructType
                                     ((struct_fields
                                       ((integer ((field_type IntegerType)))))
                                      (struct_id <opaque>)))))
                                  integer)))
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
                                    (struct_id <opaque>))))))
                               (b
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((integer ((field_type IntegerType)))))
                                    (struct_id <opaque>))))))))
                             (struct_id <opaque>)))))
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
                    (struct_id <opaque>))))))
               (b
                ((field_type
                  (StructType
                   ((struct_fields ((integer ((field_type IntegerType)))))
                    (struct_id <opaque>))))))))
             (struct_id <opaque>))))))))
      (infos ())
      (def_infos
       (((Type
          (StructType
           ((struct_fields
             ((a
               ((field_type
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_id <opaque>))))))
              (b
               ((field_type
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_id <opaque>))))))))
            (struct_id <opaque>))))
         ()))))) |}]

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
       ((var
         (Value
          (Struct
           (((struct_fields ((a ((field_type IntegerType)))))
             (struct_id <opaque>))
            ((a (Value (Integer 10))))))))
        (check
         (Value
          (Function
           ((function_signature
             ((function_params
               ((y
                 (StructType
                  ((struct_fields ((a ((field_type IntegerType)))))
                   (struct_id <opaque>))))))
              (function_returns
               (StructType
                ((struct_fields ((a ((field_type IntegerType)))))
                 (struct_id <opaque>))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (y
                     (StructType
                      ((struct_fields ((a ((field_type IntegerType)))))
                       (struct_id <opaque>))))))))))))))))
        (Value
         (Value
          (Type
           (StructType
            ((struct_fields ((a ((field_type IntegerType)))))
             (struct_id <opaque>))))))))
      (infos ())
      (def_infos
       (((Type
          (StructType
           ((struct_fields ((a ((field_type IntegerType)))))
            (struct_id <opaque>))))
         ((methods
           ((from
             ((function_signature
               ((function_params ((x IntegerType)))
                (function_returns
                 (StructType
                  ((struct_fields ((a ((field_type IntegerType)))))
                   (struct_id <opaque>))))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value
                      (Struct
                       (((struct_fields ((a ((field_type IntegerType)))))
                         (struct_id <opaque>))
                        ((a (Reference (x IntegerType))))))))))))))))))
          (impls
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
                     (function_returns
                      (StructType
                       ((struct_fields ((a ((field_type IntegerType)))))
                        (struct_id <opaque>))))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct
                            (((struct_fields ((a ((field_type IntegerType)))))
                              (struct_id <opaque>))
                             ((a (Reference (x IntegerType))))))))))))))))))))))))))))) |}]
