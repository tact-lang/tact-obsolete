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
            (function_returns Hole) (function_impl (BuiltinFn (<fun> <opaque>)))))))
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_params ((integer (Value (Type IntegerType)))))
            (function_returns Hole) (function_impl (BuiltinFn (<fun> <opaque>))))))))))) |}]
