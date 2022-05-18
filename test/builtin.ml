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
           (((struct_modifiers ((is_builtin_int true) (int_bits 8)))
             (struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_methods
              ((new
                ((function_params ((integer (Value (Type IntegerType)))))
                 (function_returns Hole) (function_impl (BuiltinFn (<fun> 7)))))))
             (struct_id <opaque>))
            ((integer (Integer 1)))))))
        (i
         (Value
          (StructInstance
           (((struct_modifiers ((is_builtin_int true) (int_bits 257)))
             (struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_methods
              ((new
                ((function_params ((integer (Value (Type IntegerType)))))
                 (function_returns Hole) (function_impl (BuiltinFn (<fun> 1)))))))
             (struct_id <opaque>))
            ((integer (Integer 100))))))))))) |}]
