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
             ((Value (Type (StructType 33))) ((value (Value (Integer 513))))))))
          (i
           (Value
            (Struct
             ((Value (Type (StructType 118))) ((value (Value (Integer 100))))))))))
        (structs
         ((119
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 118))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 7)) (v (StructType 118))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 119)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 118)))))))))))))))
              (uty_impls ()) (uty_id 119) (uty_base_id -500)))))
          (118
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 7))) (Value (Integer 257)))
                           false)))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 119)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 118)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 118)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 118))) value IntegerType))
                        (Value (Integer 257)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 118)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 118))) value IntegerType))
                           (Value (Integer 257)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 119))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 7)))
                               (Value (Integer 257)))
                              false)))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 119)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 118)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 17)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 118))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 118)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 118) (uty_base_id 16)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs (0 ())) (attr_executors <opaque>))) |}]

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
                       ((Value (Type (StructType 118)))
                        ((value (Value (Integer 100))))))))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((ResolvedReference (i <opaque>))
                      (Reference (b (StructType 3))))
                     false)))))))))))))
        (structs
         ((119
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 118))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 7)) (v (StructType 118))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 119)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 118)))))))))))))))
              (uty_impls ()) (uty_id 119) (uty_base_id -500)))))
          (118
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 7))) (Value (Integer 32)))
                           false)))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 119)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 118)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 118)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 118))) value IntegerType))
                        (Value (Integer 32)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 118)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 118))) value IntegerType))
                           (Value (Integer 32)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 119))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 7))) (Value (Integer 32)))
                              false)))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 119)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 118)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 17)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 118))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 118)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 118) (uty_base_id 16)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "demo struct serializer" =
  let source =
    {|
      struct T {
        val a: Int[32]
        val b: Int[16]
      }
      let T_serializer = serializer[T];

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
                   ((b
                     (FunctionCall ((ResolvedReference (new <opaque>)) () false)))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (T_serializer <opaque>))
                     ((Value
                       (Struct
                        ((Value (Type (StructType 123)))
                         ((a
                           (Value
                            (Struct
                             ((Value (Type (StructType 118)))
                              ((value (Value (Integer 0))))))))
                          (b
                           (Value
                            (Struct
                             ((Value (Type (StructType 120)))
                              ((value (Value (Integer 1))))))))))))
                      (Reference (b (StructType 3))))
                     false)))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((self (StructType 123)) (b (StructType 3))))
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
                             ((self (StructType 118)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 118))) value
                                  IntegerType))
                                (Value (Integer 32)))
                               false))))))))
                       ((StructField
                         ((Reference (self (StructType 123))) a (StructType 118)))
                        (Reference (b (StructType 3))))
                       false)))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 120)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 120))) value
                                  IntegerType))
                                (Value (Integer 16)))
                               false))))))))
                       ((StructField
                         ((Reference (self (StructType 123))) b (StructType 120)))
                        (Reference (b (StructType 3))))
                       false)))))
                  (Return (Reference (b (StructType 3))))))))))))
          (T (Value (Type (StructType 123))))))
        (structs
         ((123
           ((struct_fields
             ((a ((field_type (StructType 118))))
              (b ((field_type (StructType 120))))))
            (struct_details
             ((uty_methods ()) (uty_impls ()) (uty_id 123) (uty_base_id 122)))))
          (121
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 120))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 7)) (v (StructType 120))))
                    (function_returns (StructType 121))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 121)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 120)))))))))))))))
              (uty_impls ()) (uty_id 121) (uty_base_id -500)))))
          (120
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 120))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 120)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 121))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 7))) (Value (Integer 16)))
                           false)))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 121)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 120)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 120)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 120))) value IntegerType))
                        (Value (Integer 16)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 120))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 120)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 120)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 120))) value IntegerType))
                           (Value (Integer 16)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 121))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 7))) (Value (Integer 16)))
                              false)))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 121)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 120)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 17)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 120))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 120)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 120) (uty_base_id 16)))))
          (119
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 118))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 7)) (v (StructType 118))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 119)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 118)))))))))))))))
              (uty_impls ()) (uty_id 119) (uty_base_id -500)))))
          (118
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 7))) (Value (Integer 32)))
                           false)))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 119)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 118)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 118)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 118))) value IntegerType))
                        (Value (Integer 32)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 118))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 118)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 118)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 118))) value IntegerType))
                           (Value (Integer 32)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 119))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 7))) (Value (Integer 32)))
                              false)))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 119)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 118)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 17)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 118))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 118)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 118) (uty_base_id 16)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields
             ((a (Value (Type (StructType 118))))
              (b (Value (Type (StructType 120))))))
            (st_sig_methods ()) (st_sig_base_id 122) (st_sig_id 45)))))
        (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "from interface" =
  let source =
    {|
      struct Value {
        val a: Integer
        impl From[Integer] {
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
            (Struct ((Value (Type (StructType 119))) ((a (Value (Integer 10))))))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params ((y (StructType 119))))
                (function_returns (StructType 119))))
              (function_impl (Fn (Return (Reference (y (StructType 119))))))))))
          (Value (Value (Type (StructType 119))))))
        (structs
         ((119
           ((struct_fields ((a ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((x IntegerType)))
                    (function_returns (StructType 119))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 119)))
                        ((a (Reference (x IntegerType))))))))))))))
              (uty_impls
               (((impl_interface 17)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((x IntegerType)))
                       (function_returns (StructType 119))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 119)))
                           ((a (Reference (x IntegerType)))))))))))))))))
              (uty_id 119) (uty_base_id 118)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields ((a (ResolvedReference (Integer <opaque>)))))
            (st_sig_methods
             ((from
               ((function_params ((x IntegerType)))
                (function_returns (ExprType (Reference (Self (StructSig 45)))))))))
            (st_sig_base_id 118) (st_sig_id 45)))))
        (union_signs (0 ())) (attr_executors <opaque>))) |}]

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
                     ((Value (Integer 10)) (Value (Integer 2))) false)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

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
                       ((Reference (cell (StructType 1)))) false)))))
                  (Let
                   ((result
                     (FunctionCall
                      ((ResolvedReference (load_int <opaque>))
                       ((Reference (slice (StructType 7))) (Value (Integer 10)))
                       false)))))
                  (Let
                   ((slice2
                     (FunctionCall
                      ((MkFunction
                        ((function_signature
                          ((function_params ((v (StructType 7))))
                           (function_returns (StructType 7))))
                         (function_impl
                          (Fn
                           (Return
                            (StructField
                             ((Reference (result (StructType 5))) slice
                              (StructType 7))))))))
                       ((StructField
                         ((Reference (result (StructType 5))) slice (StructType 7))))
                       false)))))
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
                         ((Reference (result (StructType 5))) value IntegerType)))
                       false)))))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "deserializer" =
  let source =
    {|
      struct Something {
        val value1: Int[9]
        val value2: Int[256]
      }
      let test = deserializer[Something];
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
             ((function_params ((slice (StructType 7))))
              (function_returns (StructType 120))))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((slice slice) (value value1)))
                  (destructuring_let_expr
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((s (StructType 7))))
                          (function_returns (StructType 23))))
                        (function_impl
                         (Fn
                          (Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 7)))
                                  (Value (Integer 9)))
                                 false)))))
                            (DestructuringLet
                             ((destructuring_let ((slice slice) (value value)))
                              (destructuring_let_expr
                               (Reference (res (StructType 5))))
                              (destructuring_let_rest false)))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 23)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 22)))
                                     ((value (Reference (value IntegerType))))))))))))))))))))
                     ((Reference (slice (StructType 7)))) false)))
                  (destructuring_let_rest false)))
                (DestructuringLet
                 ((destructuring_let ((slice slice) (value value2)))
                  (destructuring_let_expr
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((s (StructType 7))))
                          (function_returns (StructType 36))))
                        (function_impl
                         (Fn
                          (Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 7)))
                                  (Value (Integer 256)))
                                 false)))))
                            (DestructuringLet
                             ((destructuring_let ((slice slice) (value value)))
                              (destructuring_let_expr
                               (Reference (res (StructType 5))))
                              (destructuring_let_rest false)))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 36)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 35)))
                                     ((value (Reference (value IntegerType))))))))))))))))))))
                     ((Reference (slice (StructType 7)))) false)))
                  (destructuring_let_rest false)))
                (Return
                 (Value
                  (Struct
                   ((Value (Type (StructType 120)))
                    ((value
                      (Value
                       (Struct
                        ((Value (Type (StructType 119)))
                         ((value1 (Reference (value1 (StructType 22))))
                          (value2 (Reference (value2 (StructType 35)))))))))
                     (slice (Reference (slice (StructType 7)))))))))))))))))
        (Something (Value (Type (StructType 119))))))
      (structs
       ((120
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 119))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((s (StructType 7)) (v (StructType 119))))
                  (function_returns (StructType 120))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 120)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 119)))))))))))))))
            (uty_impls ()) (uty_id 120) (uty_base_id -500)))))
        (119
         ((struct_fields
           ((value1 ((field_type (StructType 22))))
            (value2 ((field_type (StructType 35))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 119) (uty_base_id 118)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((value1 (Value (Type (StructType 22))))
            (value2 (Value (Type (StructType 35))))))
          (st_sig_methods ()) (st_sig_base_id 118) (st_sig_id 45)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "derive Serialize" =
  let source =
    {|
      struct Something {
        val value1: Int[9]
        
        @derive
        impl Serialize {}
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((Something (Value (Type (StructType 119))))))
      (structs
       ((119
         ((struct_fields ((value1 ((field_type (StructType 22))))))
          (struct_details
           ((uty_methods
             ((serialize
               ((function_signature
                 ((function_params ((self (StructType 119)) (b (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params
                           ((self (StructType 119)) (b (StructType 3))))
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
                                       ((self (StructType 22))
                                        (builder (StructType 3))))
                                      (function_returns (StructType 3))))
                                    (function_impl
                                     (Fn
                                      (Return
                                       (FunctionCall
                                        ((ResolvedReference
                                          (serialize_int <opaque>))
                                         ((Reference (builder (StructType 3)))
                                          (StructField
                                           ((Reference (self (StructType 22)))
                                            value IntegerType))
                                          (Value (Integer 9)))
                                         false))))))))
                                 ((StructField
                                   ((Reference (self (StructType 119))) value1
                                    (StructType 22)))
                                  (Reference (b (StructType 3))))
                                 false)))))
                            (Return (Reference (b (StructType 3)))))))))))
                     ((Reference (self (StructType 119)))
                      (Reference (b (StructType 3))))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 119)) (b (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 119)) (b (StructType 3))))
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
                                          ((self (StructType 22))
                                           (builder (StructType 3))))
                                         (function_returns (StructType 3))))
                                       (function_impl
                                        (Fn
                                         (Return
                                          (FunctionCall
                                           ((ResolvedReference
                                             (serialize_int <opaque>))
                                            ((Reference (builder (StructType 3)))
                                             (StructField
                                              ((Reference (self (StructType 22)))
                                               value IntegerType))
                                             (Value (Integer 9)))
                                            false))))))))
                                    ((StructField
                                      ((Reference (self (StructType 119))) value1
                                       (StructType 22)))
                                     (Reference (b (StructType 3))))
                                    false)))))
                               (Return (Reference (b (StructType 3)))))))))))
                        ((Reference (self (StructType 119)))
                         (Reference (b (StructType 3))))
                        false))))))))))))
            (uty_id 119) (uty_base_id 118)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((value1 (Value (Type (StructType 22))))))
          (st_sig_methods
           ((serialize
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 45)))))
                (b (StructType 3))))
              (function_returns (StructType 3))))))
          (st_sig_base_id 118) (st_sig_id 45)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Deserialize Unions" =
  let source =
    {|
      union TestUnion {
        case Int[8]
        case Int[9]
      }
      let deserialize_union = deserializer[TestUnion];
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((deserialize_union
         (Value
          (Function
           ((function_signature
             ((function_params ((slice (StructType 7))))
              (function_returns (StructType 122))))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((res_discr
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params
                           ((self (StructType 7)) (bits IntegerType)))
                          (function_returns (StructType 5))))
                        (function_impl
                         (Fn
                          (Block
                           ((Let
                             ((output
                               (FunctionCall
                                ((ResolvedReference (builtin_load_uint <opaque>))
                                 ((StructField
                                   ((Reference (self (StructType 7))) s
                                    (BuiltinType Slice)))
                                  (Reference (bits IntegerType)))
                                 false)))))
                            (Let
                             ((slice
                               (Value
                                (Struct
                                 ((Value (Type (StructType 7)))
                                  ((s
                                    (StructField
                                     ((Reference (output (StructType -5))) value1
                                      (BuiltinType Slice)))))))))))
                            (Let
                             ((int
                               (StructField
                                ((Reference (output (StructType -5))) value2
                                 IntegerType)))))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 5)))
                                ((slice
                                  (FunctionCall
                                   ((ResolvedReference (believe_me <opaque>))
                                    ((Reference (slice (StructType 7)))) false)))
                                 (value (Reference (int IntegerType)))))))))))))))
                     ((Reference (slice (StructType 7))) (Value (Integer 1)))
                     false)))))
                (If
                 ((if_condition
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((i1 IntegerType) (i2 IntegerType)))
                          (function_returns BoolType)))
                        (function_impl
                         (Fn
                          (Return
                           (Primitive
                            (Prim (name __==__)
                             (exprs
                              ((Reference (i1 IntegerType))
                               (Reference (i2 IntegerType))))))))))))
                     ((StructField
                       ((Reference (res_discr (StructType 5))) value IntegerType))
                      (Value (Integer 0)))
                     false)))
                  (if_then
                   (Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((s (StructType 7))))
                               (function_returns (StructType 34))))
                             (function_impl
                              (Fn
                               (Block
                                ((Let
                                  ((res
                                    (FunctionCall
                                     ((ResolvedReference (load_int <opaque>))
                                      ((Reference (s (StructType 7)))
                                       (Value (Integer 8)))
                                      false)))))
                                 (DestructuringLet
                                  ((destructuring_let
                                    ((slice slice) (value value)))
                                   (destructuring_let_expr
                                    (Reference (res (StructType 5))))
                                   (destructuring_let_rest false)))
                                 (Return
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 34)))
                                     ((slice (Reference (slice (StructType 7))))
                                      (value
                                       (Value
                                        (Struct
                                         ((Value (Type (StructType 33)))
                                          ((value
                                            (Reference (value IntegerType))))))))))))))))))))
                          ((StructField
                            ((Reference (res_discr (StructType 5))) slice
                             (StructType 7))))
                          false)))))
                     (Return
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((s (StructType 7)) (v (UnionType 119))))
                             (function_returns (StructType 122))))
                           (function_impl
                            (Fn
                             (Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 122)))
                                 ((slice (Reference (s (StructType 7))))
                                  (value (Reference (v (UnionType 119))))))))))))))
                        ((StructField
                          ((Reference (res (StructType 122))) slice
                           (StructType 7)))
                         (StructField
                          ((Reference (res (StructType 122))) value
                           (StructType 33))))
                        true))))))
                  (if_else
                   ((Block
                     ((Let
                       ((res_discr
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params
                                 ((self (StructType 7)) (bits IntegerType)))
                                (function_returns (StructType 5))))
                              (function_impl
                               (Fn
                                (Block
                                 ((Let
                                   ((output
                                     (FunctionCall
                                      ((ResolvedReference
                                        (builtin_load_uint <opaque>))
                                       ((StructField
                                         ((Reference (self (StructType 7))) s
                                          (BuiltinType Slice)))
                                        (Reference (bits IntegerType)))
                                       false)))))
                                  (Let
                                   ((slice
                                     (Value
                                      (Struct
                                       ((Value (Type (StructType 7)))
                                        ((s
                                          (StructField
                                           ((Reference (output (StructType -5)))
                                            value1 (BuiltinType Slice)))))))))))
                                  (Let
                                   ((int
                                     (StructField
                                      ((Reference (output (StructType -5)))
                                       value2 IntegerType)))))
                                  (Return
                                   (Value
                                    (Struct
                                     ((Value (Type (StructType 5)))
                                      ((slice
                                        (FunctionCall
                                         ((ResolvedReference
                                           (believe_me <opaque>))
                                          ((Reference (slice (StructType 7))))
                                          false)))
                                       (value (Reference (int IntegerType)))))))))))))))
                           ((StructField
                             ((Reference (res_discr (StructType 5))) slice
                              (StructType 7)))
                            (Value (Integer 1)))
                           false)))))
                      (If
                       ((if_condition
                         (FunctionCall
                          ((Value
                            (Function
                             ((function_signature
                               ((function_params
                                 ((i1 IntegerType) (i2 IntegerType)))
                                (function_returns BoolType)))
                              (function_impl
                               (Fn
                                (Return
                                 (Primitive
                                  (Prim (name __==__)
                                   (exprs
                                    ((Reference (i1 IntegerType))
                                     (Reference (i2 IntegerType))))))))))))
                           ((StructField
                             ((Reference (res_discr (StructType 5))) value
                              IntegerType))
                            (Value (Integer 1)))
                           false)))
                        (if_then
                         (Block
                          ((Let
                            ((res
                              (FunctionCall
                               ((Value
                                 (Function
                                  ((function_signature
                                    ((function_params ((s (StructType 7))))
                                     (function_returns (StructType 23))))
                                   (function_impl
                                    (Fn
                                     (Block
                                      ((Let
                                        ((res
                                          (FunctionCall
                                           ((ResolvedReference
                                             (load_int <opaque>))
                                            ((Reference (s (StructType 7)))
                                             (Value (Integer 9)))
                                            false)))))
                                       (DestructuringLet
                                        ((destructuring_let
                                          ((slice slice) (value value)))
                                         (destructuring_let_expr
                                          (Reference (res (StructType 5))))
                                         (destructuring_let_rest false)))
                                       (Return
                                        (Value
                                         (Struct
                                          ((Value (Type (StructType 23)))
                                           ((slice
                                             (Reference (slice (StructType 7))))
                                            (value
                                             (Value
                                              (Struct
                                               ((Value (Type (StructType 22)))
                                                ((value
                                                  (Reference (value IntegerType))))))))))))))))))))
                                ((StructField
                                  ((Reference (res_discr (StructType 5))) slice
                                   (StructType 7))))
                                false)))))
                           (Return
                            (FunctionCall
                             ((Value
                               (Function
                                ((function_signature
                                  ((function_params
                                    ((s (StructType 7)) (v (UnionType 119))))
                                   (function_returns (StructType 122))))
                                 (function_impl
                                  (Fn
                                   (Return
                                    (Value
                                     (Struct
                                      ((Value (Type (StructType 122)))
                                       ((slice (Reference (s (StructType 7))))
                                        (value (Reference (v (UnionType 119))))))))))))))
                              ((StructField
                                ((Reference (res (StructType 122))) slice
                                 (StructType 7)))
                               (StructField
                                ((Reference (res (StructType 122))) value
                                 (StructType 22))))
                              true))))))
                        (if_else
                         ((Expr
                           (Primitive
                            (Prim (name throw) (exprs ((Value (Integer 0)))))))))))))))))))))))))
        (TestUnion (Value (Type (UnionType 119))))))
      (structs
       ((122
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (UnionType 119))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((s (StructType 7)) (v (UnionType 119))))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (UnionType 119)))))))))))))))
            (uty_impls ()) (uty_id 122) (uty_base_id -500)))))))
      (unions
       ((119
         ((union_attributes ())
          (cases
           (((StructType 22) (Discriminator 1))
            ((StructType 33) (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 120)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 33))))
                     (function_returns (UnionType 118))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 33))) 119))))))))))
              ((impl_interface 121)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 22))))
                     (function_returns (UnionType 118))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 22))) 119))))))))))))
            (uty_id 119) (uty_base_id 118)))))))
      (interfaces
       ((121
         ((interface_methods
           ((from
             ((function_params ((from (StructType 22))))
              (function_returns SelfType)))))))
        (120
         ((interface_methods
           ((from
             ((function_params ((from (StructType 33))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 33) (StructType 22))) (un_sig_methods ())
          (un_sig_base_id 118)))))
      (attr_executors <opaque>))) |}]
