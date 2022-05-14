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
               (struct_methods
                ((new
                  (BuiltinFn
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole) (function_impl (<fun> <opaque>)))))))
               (struct_id <opaque>))
              ((integer (Integer 100)))))))))
        (Let
         ((overflow
           (Value
            (StructInstance
             (((struct_fields
                ((integer ((field_type (Value (Type IntegerType)))))))
               (struct_methods
                ((new
                  (BuiltinFn
                   ((function_params ((integer (Value (Type IntegerType)))))
                    (function_returns Hole) (function_impl (<fun> <opaque>)))))))
               (struct_id <opaque>))
              ((integer (Integer 1)))))))))))
      (bindings
       ((overflow
         (Value
          (StructInstance
           (((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_methods
              ((new
                (BuiltinFn
                 ((function_params ((integer (Value (Type IntegerType)))))
                  (function_returns Hole) (function_impl (<fun> <opaque>)))))))
             (struct_id <opaque>))
            ((integer (Integer 1)))))))
        (i
         (Value
          (StructInstance
           (((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_methods
              ((new
                (BuiltinFn
                 ((function_params ((integer (Value (Type IntegerType)))))
                  (function_returns Hole) (function_impl (<fun> <opaque>)))))))
             (struct_id <opaque>))
            ((integer (Integer 100))))))))))) |}]
