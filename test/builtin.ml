open Shared

let%expect_test "Int(bits) constructor" =
  let source =
    {|
         let i = Int(257).new(100);
         let overflow = Int(8).new(513);
       |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((ResolvedReference (LoadResult <opaque>))
            ((ResolvedReference (Self <opaque>))))))
         VoidType)))
      ((bindings
        ((overflow
          (Value
           (Struct
            ((Value (Type (StructType 25))) ((value (Value (Integer 513))))))))
         (i
          (Value
           (Struct
            ((Value (Type (StructType 76))) ((value (Value (Integer 100))))))))))
       (structs
        ((77
          ((struct_fields
            ((slice ((field_type (StructType 6))))
             (value ((field_type (StructType 76))))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((s (StructType 6)) (v (StructType 76))))
                 (function_returns (StructType 77))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 77)))
                      ((slice (Reference (s (StructType 6))))
                       (value (Reference (v (StructType 76))))))))))))))))
           (struct_impls ()) (struct_id 77)))
         (76
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 76))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 76)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 76)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 76))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (StructType 77))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return
                     (Value
                      (Struct
                       ((FunctionCall
                         ((ResolvedReference (LoadResult <opaque>))
                          ((ResolvedReference (Self <opaque>)))))
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 76)))
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType))))))))))))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 76))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 76)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 76)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 76))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 77))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return
                        (Value
                         (Struct
                          ((FunctionCall
                            ((ResolvedReference (LoadResult <opaque>))
                             ((ResolvedReference (Self <opaque>)))))
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 76)))
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType)))))))))))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 76))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 76)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 76)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)
       (struct_signs ((current_id 20) (items ())))))) |}]

let%expect_test "Int(bits) serializer" =
  let source =
    {|
         fn test(b: Builder) {
           let i = Int(32).new(100);
           i.serialize(b);
         }
       |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((ResolvedReference (LoadResult <opaque>))
            ((ResolvedReference (Self <opaque>))))))
         VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((b (StructType 3))))
               (function_returns HoleType)))
             (function_impl
              (Fn
               ((Block
                 ((Let
                   ((i
                     (FunctionCall
                      ((ResolvedReference (new <opaque>))
                       ((Value (Integer 100))))))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((Reference (i (StructType 47)))
                      (Reference (b (StructType 3))))))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
       (struct_signs ((current_id 20) (items ())))))) |}]

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
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((ResolvedReference (LoadResult <opaque>))
            ((ResolvedReference (Self <opaque>))))))
         VoidType)))
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
                  (Return
                   (FunctionCall
                    ((ResolvedReference (T_serializer <opaque>))
                     ((Value
                       (Struct
                        ((Value (Type (StructType 79)))
                         ((a
                           (FunctionCall
                            ((ResolvedReference (new <opaque>))
                             ((Value (Integer 0))))))
                          (b
                           (FunctionCall
                            ((ResolvedReference (new <opaque>))
                             ((Value (Integer 1))))))))))
                      (Reference (b (StructType 3))))))))))))))))
         (T_serializer
          (Value
           (Function
            ((function_signature
              ((function_params ((self (StructType 79)) (b (StructType 3))))
               (function_returns (StructType 3))))
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
                             ((self (StructType 47)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 47))) value
                                   IntegerType))
                                 (Value (Integer 32))))))))))))
                       ((StructField
                         ((Reference (self (StructType 79))) a (StructType 47)))
                        (Reference (b (StructType 3)))))))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 76)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 76))) value
                                   IntegerType))
                                 (Value (Integer 16))))))))))))
                       ((StructField
                         ((Reference (self (StructType 79))) b (StructType 76)))
                        (Reference (b (StructType 3)))))))))
                  (Return (Reference (b (StructType 3)))))))))))))
         (T (Value (Type (StructType 79))))))
       (structs
        ((79
          ((struct_fields
            ((a ((field_type (StructType 47))))
             (b ((field_type (StructType 76))))))
           (struct_methods ()) (struct_impls ()) (struct_id 79)))
         (77
          ((struct_fields
            ((slice ((field_type (StructType 6))))
             (value ((field_type (StructType 76))))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((s (StructType 6)) (v (StructType 76))))
                 (function_returns (StructType 77))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 77)))
                      ((slice (Reference (s (StructType 6))))
                       (value (Reference (v (StructType 76))))))))))))))))
           (struct_impls ()) (struct_id 77)))
         (76
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 76))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 76)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 76)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 76))) value IntegerType))
                      (Value (Integer 16)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (StructType 77))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                    (Return
                     (Value
                      (Struct
                       ((FunctionCall
                         ((ResolvedReference (LoadResult <opaque>))
                          ((ResolvedReference (Self <opaque>)))))
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 76)))
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType))))))))))))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 76))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 76)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 76)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 76))) value IntegerType))
                         (Value (Integer 16))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 77))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                       (Return
                        (Value
                         (Struct
                          ((FunctionCall
                            ((ResolvedReference (LoadResult <opaque>))
                             ((ResolvedReference (Self <opaque>)))))
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 76)))
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType)))))))))))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 76))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 76)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 76)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)
       (struct_signs ((current_id 21) (items ())))))) |}]

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
  pp_compile source ;
  [%expect
    {|
    (StructType 77)IntegerType(Error
                               (((UnexpectedType (StructSig 0))
                                 (TypeError
                                  ((ExprType
                                    (FunctionCall
                                     ((Value
                                       (Function
                                        ((function_signature
                                          ((function_params ((T (TypeN 0))))
                                           (function_returns (StructSig 0))))
                                         (function_impl
                                          (BuiltinFn (<fun> <opaque>))))))
                                      ((ResolvedReference (Self <opaque>))))))
                                   VoidType))
                                 (UnexpectedType (StructSig 0))
                                 (TypeError
                                  ((ExprType
                                    (FunctionCall
                                     ((Value
                                       (Function
                                        ((function_signature
                                          ((function_params ((T (TypeN 0))))
                                           (function_returns (StructSig 0))))
                                         (function_impl
                                          (BuiltinFn (<fun> <opaque>))))))
                                      ((ResolvedReference (Self <opaque>))))))
                                   VoidType))
                                 (UnexpectedType (StructSig 0))
                                 (TypeError
                                  ((ExprType
                                    (FunctionCall
                                     ((Value
                                       (Function
                                        ((function_signature
                                          ((function_params ((T (TypeN 0))))
                                           (function_returns (StructSig 0))))
                                         (function_impl
                                          (BuiltinFn (<fun> <opaque>))))))
                                      ((ResolvedReference (Self <opaque>))))))
                                   VoidType))
                                 (UnexpectedType (StructSig 0))
                                 (TypeError
                                  ((ExprType
                                    (FunctionCall
                                     ((ResolvedReference (LoadResult <opaque>))
                                      ((ResolvedReference (Self <opaque>))))))
                                   VoidType)))
                                ((bindings
                                  ((var
                                    (Value
                                     (Struct
                                      ((Value (Type (StructType 77)))
                                       ((a (Value (Integer 10))))))))
                                   (check
                                    (Value
                                     (Function
                                      ((function_signature
                                        ((function_params ((y (StructType 77))))
                                         (function_returns (StructType 77))))
                                       (function_impl
                                        (Fn
                                         ((Return
                                           (Reference (y (StructType 77)))))))))))
                                   (Value (Value (Type (StructType 77))))))
                                 (structs
                                  ((77
                                    ((struct_fields
                                      ((a ((field_type IntegerType)))))
                                     (struct_methods
                                      ((from
                                        ((function_signature
                                          ((function_params ((x IntegerType)))
                                           (function_returns (StructType 77))))
                                         (function_impl
                                          (Fn
                                           ((Return
                                             (Value
                                              (Struct
                                               ((Value (Type (StructType 77)))
                                                ((a (Reference (x IntegerType)))))))))))))))
                                     (struct_impls
                                      (((impl_interface 10)
                                        (impl_methods
                                         ((from
                                           ((function_signature
                                             ((function_params ((x IntegerType)))
                                              (function_returns (StructType 77))))
                                            (function_impl
                                             (Fn
                                              ((Return
                                                (Value
                                                 (Struct
                                                  ((Value (Type (StructType 77)))
                                                   ((a
                                                     (Reference (x IntegerType))))))))))))))))))
                                     (struct_id 77)))))
                                 (type_counter <opaque>)
                                 (memoized_fcalls <opaque>)
                                 (struct_signs ((current_id 21) (items ())))))) |}]

let%expect_test "tensor2" =
  let source =
    {|
       fn test() {
         let x = builtin_divmod(10, 2);
       }
       |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((ResolvedReference (LoadResult <opaque>))
            ((ResolvedReference (Self <opaque>))))))
         VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns HoleType)))
             (function_impl
              (Fn
               ((Let
                 ((x
                   (FunctionCall
                    ((ResolvedReference (builtin_divmod <opaque>))
                     ((Value (Integer 10)) (Value (Integer 2)))))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
       (struct_signs ((current_id 20) (items ())))))) |}]

let%expect_test "slice api" =
  let source =
    {|
         fn test(cell: Cell) {
           let slice = Slice.parse(cell);
           let result = slice.load_int(10);
           let slice2: Slice = result.slice;
           let int: Integer = result.value;
         }
       |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns (StructSig 0))))
               (function_impl (BuiltinFn (<fun> <opaque>))))))
            ((ResolvedReference (Self <opaque>))))))
         VoidType))
       (UnexpectedType (StructSig 0))
       (TypeError
        ((ExprType
          (FunctionCall
           ((ResolvedReference (LoadResult <opaque>))
            ((ResolvedReference (Self <opaque>))))))
         VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((cell (StructType 1))))
               (function_returns HoleType)))
             (function_impl
              (Fn
               ((Block
                 ((Let
                   ((slice
                     (FunctionCall
                      ((ResolvedReference (parse <opaque>))
                       ((Reference (cell (StructType 1)))))))))
                  (Let
                   ((result
                     (FunctionCall
                      ((ResolvedReference (load_int <opaque>))
                       ((Reference (slice (StructType 6))) (Value (Integer 10))))))))
                  (Let
                   ((slice2
                     (FunctionCall
                      ((MkFunction
                        ((function_signature
                          ((function_params ((v (StructType 6))))
                           (function_returns (StructType 6))))
                         (function_impl
                          (Fn
                           ((Return
                             (StructField
                              ((Reference (result (StructType 5))) slice
                               (StructType 6)))))))))
                       ((StructField
                         ((Reference (result (StructType 5))) slice
                          (StructType 6)))))))))
                  (Let
                   ((int
                     (FunctionCall
                      ((MkFunction
                        ((function_signature
                          ((function_params ((v IntegerType)))
                           (function_returns IntegerType)))
                         (function_impl
                          (Fn
                           ((Return
                             (StructField
                              ((Reference (result (StructType 5))) value
                               IntegerType))))))))
                       ((StructField
                         ((Reference (result (StructType 5))) value IntegerType)))))))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
       (struct_signs ((current_id 20) (items ())))))) |}]

(* module Config = struct
     include Tact.Located.Disabled
   end

   module Syntax = Tact.Syntax.Make (Config)
   module Parser = Tact.Parser.Make (Config)
   module Lang = Tact.Lang.Make (Config)
   module Show = Tact.Show.Make (Config)
   module Interpreter = Tact.Interpreter
   module Errors = Tact.Errors
   module Zint = Tact.Zint
   module C = Tact.Compiler
   module Codegen = Tact.Codegen_func
   module Func = Tact.Func
   include Core

   type error = [Lang.error | Interpreter.error] [@@deriving sexp_of]

   let make_errors e = new Errors.errors e

   let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

   let strip_if_exists_in_other o1 o2 ~equal =
     List.filter o1 ~f:(fun o1_item -> not @@ List.exists o2 ~f:(equal o1_item))

   let strip : program:Lang.program -> previous:Lang.program -> Lang.program =
    fun ~program ~previous ->
     { program with
       bindings =
         strip_if_exists_in_other program.bindings previous.bindings
           ~equal:Lang.equal_binding;
       structs =
         strip_if_exists_in_other program.structs previous.structs
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
       unions =
         strip_if_exists_in_other program.unions previous.unions
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
       interfaces =
         strip_if_exists_in_other program.interfaces previous.interfaces
           ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2) }

   let compile_pass p prev_program errors =
     let c = new Lang.constructor ~program:prev_program errors in
     let p' = c#visit_program () p in
     p'

   let build_program ?(errors = make_errors Show.show_error)
       ?(prev_program = Lang.default_program ()) ?(_strip_defaults = true) ~codegen
       p =
     let p' = compile_pass p prev_program errors in
     let p'' = p' in
     errors#to_result p''
     |> Result.map_error ~f:(fun errors ->
            let errs = List.map errors ~f:(fun (_, err, _) -> err) in
            (errs, p'') )
     |> Result.map ~f:codegen

   let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

   and sexp_of_errors =
     sexp_of_pair (List.sexp_of_t sexp_of_error) Lang.sexp_of_program

   and print_sexp e =
     pp_sexp (Result.sexp_of_t Lang.sexp_of_program sexp_of_errors e)

   let pp_compile ?(prev_program = Lang.default_program ())
       ?(_strip_defaults = true) s =
     parse_program s
     |> build_program ~prev_program ~_strip_defaults ~codegen:(fun x -> x)
     |> print_sexp

   let%expect_test "tensor2" =
     let source =
       {|
       struct Cell {
     val c: builtin_Cell
   }

   // Do not change place of builder struct - for internal reasons
   // it should be second struct in the file.
   struct Builder {
     val b: builtin_Builder
   }

   struct Slice {
     val s: builtin_Slice
   }
         struct Int2(bits: Integer) {
           val value: Integer

           fn deserialize() -> LoadResult(Self) {}
         }
         let IN = Int2(9);
       |}
     in
     pp_compile source ; [%expect{|
       (Ok
        ((bindings
          ((IN (Value (Type (StructType 7))))
           (Int2
            (Value
             (Function
              ((function_signature
                ((function_params ((bits IntegerType)))
                 (function_returns
                  (StructSig
                   ((st_sig_fields ((value (ResolvedReference (Integer <opaque>))))))))))
               (function_impl
                (Fn
                 ((Return
                   (MkStructDef
                    ((mk_struct_fields
                      ((value (ResolvedReference (Integer <opaque>)))))
                     (mk_methods
                      ((deserialize
                        (MkFunction
                         ((function_signature
                           ((function_params ())
                            (function_returns
                             (ExprType
                              (FunctionCall
                               ((ResolvedReference (LoadResult <opaque>))
                                ((ResolvedReference (Self <opaque>)))))))))
                          (function_impl (Fn ((Block ())))))))))
                     (mk_impls ()) (mk_struct_id 6)))))))))))
           (Slice (Value (Type (StructType 5))))
           (Builder (Value (Type (StructType 3))))
           (Cell (Value (Type (StructType 1)))) (Integer (Value (Type IntegerType)))
           (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
           (Void (Value Void)) (VoidType (Value (Type VoidType)))
           (serializer
            (Value
             (Function
              ((function_signature
                ((function_params ((t (TypeN 0))))
                 (function_returns
                  (FunctionType
                   ((function_params ((t HoleType) (b (StructType 3))))
                    (function_returns (StructType 3)))))))
               (function_impl (BuiltinFn (<fun> <opaque>)))))))
           (Serialize (Value (Type (InterfaceType -1))))
           (Deserialize (Value (Type (InterfaceType -3))))
           (LoadResult
            (Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0))))
                 (function_returns
                  (StructSig
                   ((st_sig_fields
                     ((slice (Value (Type (StructType 6))))
                      (value (Value (Type (Dependent T (TypeN 0))))))))))))
               (function_impl (BuiltinFn (<fun> <opaque>)))))))
           (From
            (Value
             (Function
              ((function_signature
                ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
               (function_impl (BuiltinFn (<fun> <opaque>)))))))
           (builtin_Builder (Value (Type (BuiltinType Builder))))
           (builtin_Cell (Value (Type (BuiltinType Cell))))
           (builtin_Slice (Value (Type (BuiltinType Slice))))
           (builtin_builder_new
            (Value
             (Function
              ((function_signature
                ((function_params ()) (function_returns (BuiltinType Builder))))
               (function_impl (Fn ((Return (Primitive EmptyBuilder)))))))))
           (builtin_builder_build
            (Value
             (Function
              ((function_signature
                ((function_params ((b (BuiltinType Builder))))
                 (function_returns (BuiltinType Cell))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (BuildCell (builder (Reference (b (BuiltinType Builder))))))))))))))
           (builtin_builder_store_int
            (Value
             (Function
              ((function_signature
                ((function_params
                  ((b (BuiltinType Builder)) (int IntegerType) (bits IntegerType)))
                 (function_returns (BuiltinType Builder))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreInt (builder (Reference (b (BuiltinType Builder))))
                     (length (Reference (bits IntegerType)))
                     (integer (Reference (int IntegerType))) (signed true)))))))))))
           (builtin_builder_store_coins
            (Value
             (Function
              ((function_signature
                ((function_params ((b (BuiltinType Builder)) (c IntegerType)))
                 (function_returns BoolType)))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreCoins (builder (Reference (b (BuiltinType Builder))))
                     (coins (Reference (c IntegerType)))))))))))))
           (builtin_slice_begin_parse
            (Value
             (Function
              ((function_signature
                ((function_params ((c (BuiltinType Cell))))
                 (function_returns (BuiltinType Slice))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive (ParseCell (cell (Reference (c (BuiltinType Cell))))))))))))))
           (builtin_slice_end_parse
            (Value
             (Function
              ((function_signature
                ((function_params ((s (BuiltinType Slice))))
                 (function_returns VoidType)))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (SliceEndParse (slice (Reference (s (BuiltinType Slice))))))))))))))
           (builtin_slice_load_int
            (Value
             (Function
              ((function_signature
                ((function_params ((s (BuiltinType Slice)) (bits IntegerType)))
                 (function_returns (StructType -5))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (SliceLoadInt (slice (Reference (s (BuiltinType Slice))))
                     (bits (Reference (bits IntegerType)))))))))))))
           (builtin_divmod
            (Value
             (Function
              ((function_signature
                ((function_params ((x IntegerType) (y IntegerType)))
                 (function_returns (StructType -4))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (Divmod (x (Reference (x IntegerType)))
                     (y (Reference (y IntegerType)))))))))))))
           (builtin_send_raw_msg
            (Value
             (Function
              ((function_signature
                ((function_params ((msg (BuiltinType Cell)) (flags IntegerType)))
                 (function_returns VoidType)))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (SendRawMsg (msg (Reference (msg (BuiltinType Cell))))
                     (flags (Reference (flags IntegerType)))))))))))))
           (builtin_equal
            (Value
             (Function
              ((function_signature
                ((function_params ((x IntegerType) (y IntegerType)))
                 (function_returns BoolType)))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (Equality (x (Reference (x IntegerType)))
                     (y (Reference (y IntegerType)))))))))))))))
         (structs
          ((8
            ((struct_fields
              ((slice ((field_type (StructType 6))))
               (value ((field_type (StructType 7))))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((s (StructType 6)) (v (StructType 7))))
                   (function_returns (StructType 8))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 8)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 7))))))))))))))))
             (struct_impls ()) (struct_id 8)))
           (7
            ((struct_fields ((value ((field_type IntegerType)))))
             (struct_methods
              ((deserialize
                ((function_signature
                  ((function_params ()) (function_returns (StructType 8))))
                 (function_impl (Fn ((Block ()))))))))
             (struct_impls ()) (struct_id 7)))
           (5
            ((struct_fields ((s ((field_type (BuiltinType Slice))))))
             (struct_methods ()) (struct_impls ()) (struct_id 5)))
           (3
            ((struct_fields ((b ((field_type (BuiltinType Builder))))))
             (struct_methods ()) (struct_impls ()) (struct_id 3)))
           (1
            ((struct_fields ((c ((field_type (BuiltinType Cell))))))
             (struct_methods ()) (struct_impls ()) (struct_id 1)))
           (-4
            ((struct_fields
              ((value1 ((field_type IntegerType)))
               (value2 ((field_type IntegerType)))))
             (struct_methods ()) (struct_impls ()) (struct_id -4) (tensor)))
           (-5
            ((struct_fields
              ((value1 ((field_type (BuiltinType Slice))))
               (value2 ((field_type IntegerType)))))
             (struct_methods ()) (struct_impls ()) (struct_id -5) (tensor)))
           (-2
            ((struct_fields
              ((slice ((field_type (StructType 6))))
               (value ((field_type SelfType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((s (StructType 6)) (v SelfType)))
                   (function_returns (StructType -2))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value
                      (Struct
                       ((Value (Type (StructType -2)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v SelfType)))))))))))))))
             (struct_impls ()) (struct_id -2)))))
         (interfaces
          ((-1
            ((interface_methods
              ((serialize
                ((function_params ((self SelfType) (b (StructType 3))))
                 (function_returns (StructType 3))))))))
           (-3
            ((interface_methods
              ((deserialize
                ((function_params ((b (StructType 3))))
                 (function_returns (StructType -2))))))))))
         (type_counter <opaque>) (memoized_fcalls <opaque>))) |}] *)
