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
             ((Value (Type (StructType 27))) ((value (Value (Integer 513))))))))
          (i
           (Value
            (Struct
             ((Value (Type (StructType 101))) ((value (Value (Integer 100))))))))))
        (structs
         ((102
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 101))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 101))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 102)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 101)))))))))))))))
              (uty_impls ()) (uty_id 102) (uty_base_id -500)))))
          (101
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 101)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 101))) value IntegerType))
                        (Value (Integer 257))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 102)))
                          ((slice (Reference (slice (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 101)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 101)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 101))) value IntegerType))
                           (Value (Integer 257)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 102))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6)))
                               (Value (Integer 257))))))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 102)))
                             ((slice (Reference (slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 101)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 12)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 101))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 101)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 101) (uty_base_id 11)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs (0 ())))) |}]

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
                       ((Value (Type (StructType 101)))
                        ((value (Value (Integer 100))))))))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((ResolvedReference (i <opaque>))
                      (Reference (b (StructType 3)))))))))))))))))
        (structs
         ((102
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 101))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 101))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 102)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 101)))))))))))))))
              (uty_impls ()) (uty_id 102) (uty_base_id -500)))))
          (101
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 101)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 101))) value IntegerType))
                        (Value (Integer 32))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 32))))))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 102)))
                          ((slice (Reference (slice (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 101)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 101)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 101))) value IntegerType))
                           (Value (Integer 32)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 102))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6))) (Value (Integer 32))))))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 102)))
                             ((slice (Reference (slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 101)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 12)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 101))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 101)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 101) (uty_base_id 11)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
        (union_signs (0 ())))) |}]

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
                        ((Value (Type (StructType 106)))
                         ((a
                           (Value
                            (Struct
                             ((Value (Type (StructType 101)))
                              ((value (Value (Integer 0))))))))
                          (b
                           (Value
                            (Struct
                             ((Value (Type (StructType 103)))
                              ((value (Value (Integer 1))))))))))))
                      (Reference (b (StructType 3)))))))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((self (StructType 106)) (b (StructType 3))))
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
                             ((self (StructType 101)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 101))) value
                                  IntegerType))
                                (Value (Integer 32)))))))))))
                       ((StructField
                         ((Reference (self (StructType 106))) a (StructType 101)))
                        (Reference (b (StructType 3)))))))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 103)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 103))) value
                                  IntegerType))
                                (Value (Integer 16)))))))))))
                       ((StructField
                         ((Reference (self (StructType 106))) b (StructType 103)))
                        (Reference (b (StructType 3)))))))))
                  (Return (Reference (b (StructType 3))))))))))))
          (T (Value (Type (StructType 106))))))
        (structs
         ((106
           ((struct_fields
             ((a ((field_type (StructType 101))))
              (b ((field_type (StructType 103))))))
            (struct_details
             ((uty_methods ()) (uty_impls ()) (uty_id 106) (uty_base_id 105)))))
          (104
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 103))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 103))))
                    (function_returns (StructType 104))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 104)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 103)))))))))))))))
              (uty_impls ()) (uty_id 104) (uty_base_id -500)))))
          (103
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 103))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 103)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 103)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 103))) value IntegerType))
                        (Value (Integer 16))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 104))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 104)))
                          ((slice (Reference (slice (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 103)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 103))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 103)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 103)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 103))) value IntegerType))
                           (Value (Integer 16)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 104))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6))) (Value (Integer 16))))))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 104)))
                             ((slice (Reference (slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 103)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 12)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 103))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 103)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 103) (uty_base_id 11)))))
          (102
           ((struct_fields
             ((slice ((field_type (StructType 6))))
              (value ((field_type (StructType 101))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((s (StructType 6)) (v (StructType 101))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 102)))
                        ((slice (Reference (s (StructType 6))))
                         (value (Reference (v (StructType 101)))))))))))))))
              (uty_impls ()) (uty_id 102) (uty_base_id -500)))))
          (101
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 101)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 101))) value IntegerType))
                        (Value (Integer 32))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Block
                     ((Let
                       ((res
                         (FunctionCall
                          ((ResolvedReference (load_int <opaque>))
                           ((Reference (s (StructType 6))) (Value (Integer 32))))))))
                      (DestructuringLet
                       ((destructuring_let ((slice slice) (value value)))
                        (destructuring_let_expr (Reference (res (StructType 5))))
                        (destructuring_let_rest false)))
                      (Return
                       (Value
                        (Struct
                         ((Value (Type (StructType 102)))
                          ((slice (Reference (slice (StructType 6))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 101)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 101))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 101)))
                        ((value (Reference (i IntegerType))))))))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 101)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 101))) value IntegerType))
                           (Value (Integer 32)))))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 6))))
                       (function_returns (StructType 102))))
                     (function_impl
                      (Fn
                       (Block
                        ((Let
                          ((res
                            (FunctionCall
                             ((ResolvedReference (load_int <opaque>))
                              ((Reference (s (StructType 6))) (Value (Integer 32))))))))
                         (DestructuringLet
                          ((destructuring_let ((slice slice) (value value)))
                           (destructuring_let_expr
                            (Reference (res (StructType 5))))
                           (destructuring_let_rest false)))
                         (Return
                          (Value
                           (Struct
                            ((Value (Type (StructType 102)))
                             ((slice (Reference (slice (StructType 6))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 101)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 12)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 101))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 101)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 101) (uty_base_id 11)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields
             ((a (Value (Type (StructType 101))))
              (b (Value (Type (StructType 103))))))
            (st_sig_methods ()) (st_sig_base_id 105) (st_sig_id 135)))))
        (union_signs (0 ())))) |}]

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
            (Struct ((Value (Type (StructType 102))) ((a (Value (Integer 10))))))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params ((y (StructType 102))))
                (function_returns (StructType 102))))
              (function_impl (Fn (Return (Reference (y (StructType 102))))))))))
          (Value (Value (Type (StructType 102))))))
        (structs
         ((102
           ((struct_fields ((a ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((x IntegerType)))
                    (function_returns (StructType 102))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 102)))
                        ((a (Reference (x IntegerType))))))))))))))
              (uty_impls
               (((impl_interface 12)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((x IntegerType)))
                       (function_returns (StructType 102))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 102)))
                           ((a (Reference (x IntegerType)))))))))))))))))
              (uty_id 102) (uty_base_id 101)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields ((a (ResolvedReference (Integer <opaque>)))))
            (st_sig_methods
             ((from
               ((function_params ((x IntegerType)))
                (function_returns (ExprType (Reference (Self (StructSig 135)))))))))
            (st_sig_base_id 101) (st_sig_id 135)))))
        (union_signs (0 ())))) |}]

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
        (struct_signs (0 ())) (union_signs (0 ())))) |}]

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
        (struct_signs (0 ())) (union_signs (0 ())))) |}]

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
             ((function_params ((slice (StructType 6))))
              (function_returns (StructType 103))))
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
                         ((function_params ((s (StructType 6))))
                          (function_returns (StructType 17))))
                        (function_impl
                         (Fn
                          (Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6)))
                                  (Value (Integer 9))))))))
                            (DestructuringLet
                             ((destructuring_let ((slice slice) (value value)))
                              (destructuring_let_expr
                               (Reference (res (StructType 5))))
                              (destructuring_let_rest false)))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 17)))
                                ((slice (Reference (slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 16)))
                                     ((value (Reference (value IntegerType))))))))))))))))))))
                     ((Reference (slice (StructType 6)))))))
                  (destructuring_let_rest false)))
                (DestructuringLet
                 ((destructuring_let ((slice slice) (value value2)))
                  (destructuring_let_expr
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((s (StructType 6))))
                          (function_returns (StructType 30))))
                        (function_impl
                         (Fn
                          (Block
                           ((Let
                             ((res
                               (FunctionCall
                                ((ResolvedReference (load_int <opaque>))
                                 ((Reference (s (StructType 6)))
                                  (Value (Integer 256))))))))
                            (DestructuringLet
                             ((destructuring_let ((slice slice) (value value)))
                              (destructuring_let_expr
                               (Reference (res (StructType 5))))
                              (destructuring_let_rest false)))
                            (Return
                             (Value
                              (Struct
                               ((Value (Type (StructType 30)))
                                ((slice (Reference (slice (StructType 6))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 29)))
                                     ((value (Reference (value IntegerType))))))))))))))))))))
                     ((Reference (slice (StructType 6)))))))
                  (destructuring_let_rest false)))
                (Return
                 (Value
                  (Struct
                   ((Value (Type (StructType 103)))
                    ((value
                      (Value
                       (Struct
                        ((Value (Type (StructType 102)))
                         ((value1 (Reference (value1 (StructType 16))))
                          (value2 (Reference (value2 (StructType 29)))))))))
                     (slice (Reference (slice (StructType 6)))))))))))))))))
        (Something (Value (Type (StructType 102))))))
      (structs
       ((103
         ((struct_fields
           ((slice ((field_type (StructType 6))))
            (value ((field_type (StructType 102))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((s (StructType 6)) (v (StructType 102))))
                  (function_returns (StructType 103))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 103)))
                      ((slice (Reference (s (StructType 6))))
                       (value (Reference (v (StructType 102)))))))))))))))
            (uty_impls ()) (uty_id 103) (uty_base_id -500)))))
        (102
         ((struct_fields
           ((value1 ((field_type (StructType 16))))
            (value2 ((field_type (StructType 29))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 102) (uty_base_id 101)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((value1 (Value (Type (StructType 16))))
            (value2 (Value (Type (StructType 29))))))
          (st_sig_methods ()) (st_sig_base_id 101) (st_sig_id 135)))))
      (union_signs (0 ())))) |}]
