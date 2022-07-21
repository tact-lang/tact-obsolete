open Shared.Disabled
module Config = Shared.DisabledConfig

let%expect_test "Int[bits] constructor" =
  let source =
    {|
      let i = Int[257].new(100);
      let overflow = Int[8].new(513);
    |}
  in
  pp_compile source ;
  [%expect
    {|
      (Ok
       ((bindings
         ((overflow
           (Value
            (Struct
             ((Value (Type (StructType 26))) ((value (Value (Integer 513))))))))
          (i
           (Value
            (Struct
             ((Value (Type (StructType 83))) ((value (Value (Integer 100))))))))))
        (structs
         ((84
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 83))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 83))))
                    (function_returns (StructType 84))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 84)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 83)))))))))))))))
              (uty_impls ()) (uty_id 84) (uty_base_id -500)))))
          (83
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 83))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 83)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 83)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 83))) value IntegerType))
                        (Value (Integer 257))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 84))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 84)))
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 83)))
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 83))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 83)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 83)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 83))) value IntegerType))
                           (Value (Integer 257)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 84))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6)))
                               (Value (Integer 257))))))))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 84)))
                             ((slice
                               (StructField
                                ((Reference (res (StructType 5))) slice
                                 (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 83)))
                                  ((value
                                    (StructField
                                     ((Reference (res (StructType 5))) value
                                      IntegerType))))))))))))))))))))))
                ((impl_interface 10)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 83))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 83)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 83) (uty_base_id 9)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]

let%expect_test "Int[bits] serializer" =
  let source =
    {|
      fn test(b: Builder) {
        let i = Int[32].new(100);
        i.serialize(b);
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
      (Ok
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ((b (StructType 3)))) (function_returns HoleType)))
              (function_impl
               (Fn
                (Block
                 ((Let
                   ((i
                     (Value
                      (Struct
                       ((Value (Type (StructType 52)))
                        ((value (Value (Integer 100))))))))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((ResolvedReference (i <opaque>))
                      (Reference (b (StructType 3)))))))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
      struct T {
        val a: Int[32]
        val b: Int[16]
      }
      let T_serializer = serializer(T);

      fn test() {
        let b = Builder.new();
        T_serializer(T{a: Int[32].new(0), b: Int[16].new(1)}, b);
      }
    |}
  in
  pp_compile source ;
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
                (Block
                 ((Let
                   ((b (FunctionCall ((ResolvedReference (new <opaque>)) ())))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (T_serializer <opaque>))
                     ((Value
                       (Struct
                        ((Value (Type (StructType 86)))
                         ((a
                           (Value
                            (Struct
                             ((Value (Type (StructType 52)))
                              ((value (Value (Integer 0))))))))
                          (b
                           (Value
                            (Struct
                             ((Value (Type (StructType 83)))
                              ((value (Value (Integer 1))))))))))))
                      (Reference (b (StructType 3)))))))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((self (StructType 86)) (b (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                (Block
                 ((Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 52)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 52))) value
                                  IntegerType))
                                (Value (Integer 32)))))))))))
                       ((StructField
                         ((Reference (self (StructType 86))) a (StructType 52)))
                        (Reference (b (StructType 3)))))))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 83)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 83))) value
                                  IntegerType))
                                (Value (Integer 16)))))))))))
                       ((StructField
                         ((Reference (self (StructType 86))) b (StructType 83)))
                        (Reference (b (StructType 3)))))))))
                  (Return (Reference (b (StructType 3))))))))))))
          (T (Value (Type (StructType 86))))))
        (structs
         ((86
           ((struct_fields
             ((a ((field_type (StructType 52))))
              (b ((field_type (StructType 83))))))
            (struct_details
             ((uty_methods ()) (uty_impls ()) (uty_id 86) (uty_base_id 85)))))
          (84
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 83))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 83))))
                    (function_returns (StructType 84))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 84)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 83)))))))))))))))
              (uty_impls ()) (uty_id 84) (uty_base_id -500)))))
          (83
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 83))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 83)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 83)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 83))) value IntegerType))
                        (Value (Integer 16))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 84))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 84)))
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 83)))
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 83))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 83)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 83)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 83))) value IntegerType))
                           (Value (Integer 16)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 84))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 84)))
                             ((slice
                               (StructField
                                ((Reference (res (StructType 5))) slice
                                 (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 83)))
                                  ((value
                                    (StructField
                                     ((Reference (res (StructType 5))) value
                                      IntegerType))))))))))))))))))))))
                ((impl_interface 10)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 83))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 83)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 83) (uty_base_id 9)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]

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
      (Ok
       ((bindings
         ((var
           (Value
            (Struct ((Value (Type (StructType 84))) ((a (Value (Integer 10))))))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params ((y (StructType 84))))
                (function_returns (StructType 84))))
              (function_impl (Fn (Return (Reference (y (StructType 84))))))))))
          (Value (Value (Type (StructType 84))))))
        (structs
         ((84
           ((struct_fields ((a ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((x IntegerType)))
                    (function_returns (StructType 84))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 84)))
                        ((a (Reference (x IntegerType))))))))))))))
              (uty_impls
               (((impl_interface 10)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((x IntegerType)))
                       (function_returns (StructType 84))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 84)))
                           ((a (Reference (x IntegerType)))))))))))))))))
              (uty_id 84) (uty_base_id 83)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]

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
      (Ok
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ()) (function_returns HoleType)))
              (function_impl
               (Fn
                (Let
                 ((x
                   (FunctionCall
                    ((ResolvedReference (builtin_divmod <opaque>))
                     ((Value (Integer 10)) (Value (Integer 2))))))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]

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
      (Ok
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ((cell (StructType 1))))
                (function_returns HoleType)))
              (function_impl
               (Fn
                (Block
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
                           (Return
                            (StructField
                             ((Reference (result (StructType 5))) slice
                              (StructType 6))))))))
                       ((StructField
                         ((Reference (result (StructType 5))) slice (StructType 6)))))))))
                  (Let
                   ((int
                     (FunctionCall
                      ((MkFunction
                        ((function_signature
                          ((function_params ((v IntegerType)))
                           (function_returns IntegerType)))
                         (function_impl
                          (Fn
                           (Return
                            (StructField
                             ((Reference (result (StructType 5))) value
                              IntegerType)))))))
                       ((StructField
                         ((Reference (result (StructType 5))) value IntegerType))))))))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ()))
        (union_signs
         (5
          (((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
            (un_sig_base_id 77))
           ((un_sig_cases ((StructType 55))) (un_sig_methods ())
            (un_sig_base_id 60))
           ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
            (un_sig_base_id 44))
           ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
            (un_sig_base_id 38))
           ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
            (un_sig_base_id 20))))))) |}]
