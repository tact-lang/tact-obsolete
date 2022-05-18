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
     ((stmts
       ((Let
         ((i
           (Value
            (StructInstance
             (((struct_fields
                ((integer ((field_type (Value (Type IntegerType)))))))
               (struct_id <opaque>))
              ((integer (Integer 100)))))))))
        (Let
         ((overflow
           (Value
            (StructInstance
             (((struct_fields
                ((integer ((field_type (Value (Type IntegerType)))))))
               (struct_id <opaque>))
              ((integer (Integer 1)))))))))))
      (bindings
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
           ((function_params ((integer (Value (Type IntegerType)))))
            (function_returns
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_id <opaque>)))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_params
             ((self
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))
              (builder (Value (Type (BuiltinType Builder))))))
            (function_returns (Value (Type VoidType)))
            (function_impl (BuiltinFn (<fun> <opaque>)))))))
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_params ((integer (Value (Type IntegerType)))))
            (function_returns
             (Value
              (Struct
               ((struct_fields
                 ((integer ((field_type (Value (Type IntegerType)))))))
                (struct_id <opaque>)))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_params
             ((self
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))
              (builder (Value (Type (BuiltinType Builder))))))
            (function_returns (Value (Type VoidType)))
            (function_impl (BuiltinFn (<fun> <opaque>))))))))))) |}]

let%expect_test "Int(bits) serializator" =
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
       ((stmts
         ((Let
           ((test
             (Value
              (Function
               ((function_params ((b (Value (Type (BuiltinType Builder))))))
                (function_returns Hole)
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
                     (Asm
                      (((StructField
                         ((Value
                           (StructInstance
                            (((struct_fields
                               ((integer ((field_type (Value (Type IntegerType)))))))
                              (struct_id <opaque>))
                             ((integer (Integer 100))))))
                          integer))
                        (Reference (b (BuiltinType Builder))))
                       ((PUSHINT 32) STIX))))))))))))))))
        (bindings
         ((test
           (Value
            (Function
             ((function_params ((b (Value (Type (BuiltinType Builder))))))
              (function_returns Hole)
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
                   (Asm
                    (((StructField
                       ((Value
                         (StructInstance
                          (((struct_fields
                             ((integer ((field_type (Value (Type IntegerType)))))))
                            (struct_id <opaque>))
                           ((integer (Integer 100))))))
                        integer))
                      (Reference (b (BuiltinType Builder))))
                     ((PUSHINT 32) STIX))))))))))))))
        (methods
         (((Struct
            ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))
           ((new
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_params
               ((self
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type VoidType)))
              (function_impl (BuiltinFn (<fun> <opaque>))))))))))) |}]
