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
             ((Value (Type (StructType 35))) ((value (Value (Integer 513))))))))
          (i
           (Value
            (Struct
             ((Value (Type (StructType 134))) ((value (Value (Integer 100))))))))))
        (structs
         ((135
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 134))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((v (StructType 134)) (s (StructType 7))))
                    (function_returns (StructType 135))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 135)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 134)))))))))))))))
              (uty_impls ()) (uty_id 135) (uty_base_id -500)))))
          (134
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 135))))
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
                         ((Value (Type (StructType 135)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 134)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (Value (Integer 257)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value (Reference (i IntegerType))))))))))))
                (add
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (add <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (sub
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (sub <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (mul
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (mul <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (div
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (div <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (bit_and
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (bit_and <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (bit_or
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (bit_or <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (eq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (eq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (neq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (neq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (leq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (leq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (lt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (lt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (lt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (lt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (geq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (geq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (gt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (gt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 134)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 134))) value IntegerType))
                           (Value (Integer 257)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 135))))
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
                            ((Value (Type (StructType 135)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 134)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 19)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 134))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 134)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 134) (uty_base_id 18)))))))
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
                       ((Value (Type (StructType 43)))
                        ((value (Value (Integer 100))))))))))
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize <opaque>))
                     ((ResolvedReference (i <opaque>))
                      (Reference (b (StructType 3))))
                     false)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

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
                        ((Value (Type (StructType 137)))
                         ((a
                           (Value
                            (Struct
                             ((Value (Type (StructType 43)))
                              ((value (Value (Integer 0))))))))
                          (b
                           (Value
                            (Struct
                             ((Value (Type (StructType 134)))
                              ((value (Value (Integer 1))))))))))))
                      (Reference (b (StructType 3))))
                     false)))))))))))
          (T_serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((self (StructType 137)) (b (StructType 3))))
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
                             ((self (StructType 43)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 43))) value
                                  IntegerType))
                                (Value (Integer 32)))
                               false))))))))
                       ((StructField
                         ((Reference (self (StructType 137))) a (StructType 43)))
                        (Reference (b (StructType 3))))
                       false)))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((self (StructType 134)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            (Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 134))) value
                                  IntegerType))
                                (Value (Integer 16)))
                               false))))))))
                       ((StructField
                         ((Reference (self (StructType 137))) b (StructType 134)))
                        (Reference (b (StructType 3))))
                       false)))))
                  (Return (Reference (b (StructType 3))))))))))))
          (T (Value (Type (StructType 137))))))
        (structs
         ((137
           ((struct_fields
             ((a ((field_type (StructType 43))))
              (b ((field_type (StructType 134))))))
            (struct_details
             ((uty_methods ()) (uty_impls ()) (uty_id 137) (uty_base_id 136)))))
          (135
           ((struct_fields
             ((slice ((field_type (StructType 7))))
              (value ((field_type (StructType 134))))))
            (struct_details
             ((uty_methods
               ((new
                 ((function_signature
                   ((function_params ((v (StructType 134)) (s (StructType 7))))
                    (function_returns (StructType 135))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 135)))
                        ((slice (Reference (s (StructType 7))))
                         (value (Reference (v (StructType 134)))))))))))))))
              (uty_impls ()) (uty_id 135) (uty_base_id -500)))))
          (134
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value (Reference (i IntegerType))))))))))))
                (deserialize
                 ((function_signature
                   ((function_params ((s (StructType 7))))
                    (function_returns (StructType 135))))
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
                         ((Value (Type (StructType 135)))
                          ((slice (Reference (slice (StructType 7))))
                           (value
                            (Value
                             (Struct
                              ((Value (Type (StructType 134)))
                               ((value (Reference (value IntegerType)))))))))))))))))))
                (serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (Value (Integer 16)))
                       false)))))))
                (new
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value (Reference (i IntegerType))))))))))))
                (add
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (add <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (sub
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (sub <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (mul
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (mul <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (div
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (div <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (bit_and
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (bit_and <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (bit_or
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns (StructType 134))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 134)))
                        ((value
                          (FunctionCall
                           ((ResolvedReference (bit_or <opaque>))
                            ((StructField
                              ((Reference (self (StructType 134))) value
                               IntegerType))
                             (StructField
                              ((Reference (other (StructType 134))) value
                               IntegerType)))
                            false))))))))))))
                (eq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (eq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (neq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (neq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (leq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (leq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (lt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (lt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (lt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (lt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (geq
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (geq <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))
                (gt
                 ((function_signature
                   ((function_params
                     ((self (StructType 134)) (other (StructType 134))))
                    (function_returns BoolType)))
                  (function_impl
                   (Fn
                    (Return
                     (FunctionCall
                      ((ResolvedReference (gt <opaque>))
                       ((StructField
                         ((Reference (self (StructType 134))) value IntegerType))
                        (StructField
                         ((Reference (other (StructType 134))) value IntegerType)))
                       false)))))))))
              (uty_impls
               (((impl_interface -1)
                 (impl_methods
                  ((serialize
                    ((function_signature
                      ((function_params
                        ((self (StructType 134)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       (Return
                        (FunctionCall
                         ((ResolvedReference (serialize_int <opaque>))
                          ((Reference (builder (StructType 3)))
                           (StructField
                            ((Reference (self (StructType 134))) value IntegerType))
                           (Value (Integer 16)))
                          false))))))))))
                ((impl_interface -2)
                 (impl_methods
                  ((deserialize
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 135))))
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
                            ((Value (Type (StructType 135)))
                             ((slice (Reference (slice (StructType 7))))
                              (value
                               (Value
                                (Struct
                                 ((Value (Type (StructType 134)))
                                  ((value (Reference (value IntegerType))))))))))))))))))))))
                ((impl_interface 19)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 134))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 134)))
                           ((value (Reference (i IntegerType)))))))))))))))))
              (uty_id 134) (uty_base_id 18)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields
             ((a (Value (Type (StructType 43))))
              (b (Value (Type (StructType 134))))))
            (st_sig_methods ()) (st_sig_base_id 136) (st_sig_id 60)))))
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
            (Struct ((Value (Type (StructType 135))) ((a (Value (Integer 10))))))))
          (check
           (Value
            (Function
             ((function_signature
               ((function_params ((y (StructType 135))))
                (function_returns (StructType 135))))
              (function_impl (Fn (Return (Reference (y (StructType 135))))))))))
          (Value (Value (Type (StructType 135))))))
        (structs
         ((135
           ((struct_fields ((a ((field_type IntegerType)))))
            (struct_details
             ((uty_methods
               ((from
                 ((function_signature
                   ((function_params ((x IntegerType)))
                    (function_returns (StructType 135))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 135)))
                        ((a (Reference (x IntegerType))))))))))))))
              (uty_impls
               (((impl_interface 19)
                 (impl_methods
                  ((from
                    ((function_signature
                      ((function_params ((x IntegerType)))
                       (function_returns (StructType 135))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 135)))
                           ((a (Reference (x IntegerType)))))))))))))))))
              (uty_id 135) (uty_base_id 134)))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs
         (1
          (((st_sig_fields ((a (ResolvedReference (Integer <opaque>)))))
            (st_sig_methods
             ((from
               ((function_params ((x IntegerType)))
                (function_returns (ExprType (Reference (Self (StructSig 60)))))))))
            (st_sig_base_id 134) (st_sig_id 60)))))
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
              (function_returns (StructType 136))))
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
                          (function_returns (StructType 25))))
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
                               ((Value (Type (StructType 25)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 24)))
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
                          (function_returns (StructType 38))))
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
                               ((Value (Type (StructType 38)))
                                ((slice (Reference (slice (StructType 7))))
                                 (value
                                  (Value
                                   (Struct
                                    ((Value (Type (StructType 37)))
                                     ((value (Reference (value IntegerType))))))))))))))))))))
                     ((Reference (slice (StructType 7)))) false)))
                  (destructuring_let_rest false)))
                (Return
                 (Value
                  (Struct
                   ((Value (Type (StructType 136)))
                    ((slice (Reference (slice (StructType 7))))
                     (value
                      (Value
                       (Struct
                        ((Value (Type (StructType 135)))
                         ((value1 (Reference (value1 (StructType 24))))
                          (value2 (Reference (value2 (StructType 37))))))))))))))))))))))
        (Something (Value (Type (StructType 135))))))
      (structs
       ((136
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 135))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 135)) (s (StructType 7))))
                  (function_returns (StructType 136))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 136)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 135)))))))))))))))
            (uty_impls ()) (uty_id 136) (uty_base_id -500)))))
        (135
         ((struct_fields
           ((value1 ((field_type (StructType 24))))
            (value2 ((field_type (StructType 37))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((value1 (Value (Type (StructType 24))))
            (value2 (Value (Type (StructType 37))))))
          (st_sig_methods ()) (st_sig_base_id 134) (st_sig_id 60)))))
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
     ((bindings ((Something (Value (Type (StructType 135))))))
      (structs
       ((135
         ((struct_fields ((value1 ((field_type (StructType 24))))))
          (struct_details
           ((uty_methods
             ((serialize
               ((function_signature
                 ((function_params ((self (StructType 135)) (b (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params
                           ((self (StructType 135)) (b (StructType 3))))
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
                                       ((self (StructType 24))
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
                                           ((Reference (self (StructType 24)))
                                            value IntegerType))
                                          (Value (Integer 9)))
                                         false))))))))
                                 ((StructField
                                   ((Reference (self (StructType 135))) value1
                                    (StructType 24)))
                                  (Reference (b (StructType 3))))
                                 false)))))
                            (Return (Reference (b (StructType 3)))))))))))
                     ((Reference (self (StructType 135)))
                      (Reference (b (StructType 3))))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 135)) (b (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params
                              ((self (StructType 135)) (b (StructType 3))))
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
                                          ((self (StructType 24))
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
                                              ((Reference (self (StructType 24)))
                                               value IntegerType))
                                             (Value (Integer 9)))
                                            false))))))))
                                    ((StructField
                                      ((Reference (self (StructType 135))) value1
                                       (StructType 24)))
                                     (Reference (b (StructType 3))))
                                    false)))))
                               (Return (Reference (b (StructType 3)))))))))))
                        ((Reference (self (StructType 135)))
                         (Reference (b (StructType 3))))
                        false))))))))))))
            (uty_id 135) (uty_base_id 134)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((value1 (Value (Type (StructType 24))))))
          (st_sig_methods
           ((serialize
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 60)))))
                (b (StructType 3))))
              (function_returns (StructType 3))))))
          (st_sig_base_id 134) (st_sig_id 60)))))
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
              (function_returns (StructType 138))))
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
                            (Prim (name _==_)
                             (exprs
                              ((Reference (i1 IntegerType))
                               (Reference (i2 IntegerType))))
                             (out_ty BoolType)))))))))
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
                               (function_returns (StructType 36))))
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
                                    ((Value (Type (StructType 36)))
                                     ((slice (Reference (slice (StructType 7))))
                                      (value
                                       (Value
                                        (Struct
                                         ((Value (Type (StructType 35)))
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
                              ((v (UnionType 135)) (s (StructType 7))))
                             (function_returns (StructType 138))))
                           (function_impl
                            (Fn
                             (Return
                              (Value
                               (Struct
                                ((Value (Type (StructType 138)))
                                 ((slice (Reference (s (StructType 7))))
                                  (value (Reference (v (UnionType 135))))))))))))))
                        ((StructField
                          ((Reference (res (StructType 138))) slice
                           (StructType 7)))
                         (StructField
                          ((Reference (res (StructType 138))) value
                           (StructType 35))))
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
                           ((Reference (slice (StructType 7)))
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
                                  (Prim (name _==_)
                                   (exprs
                                    ((Reference (i1 IntegerType))
                                     (Reference (i2 IntegerType))))
                                   (out_ty BoolType)))))))))
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
                                     (function_returns (StructType 25))))
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
                                          ((Value (Type (StructType 25)))
                                           ((slice
                                             (Reference (slice (StructType 7))))
                                            (value
                                             (Value
                                              (Struct
                                               ((Value (Type (StructType 24)))
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
                                    ((v (UnionType 135)) (s (StructType 7))))
                                   (function_returns (StructType 138))))
                                 (function_impl
                                  (Fn
                                   (Return
                                    (Value
                                     (Struct
                                      ((Value (Type (StructType 138)))
                                       ((slice (Reference (s (StructType 7))))
                                        (value (Reference (v (UnionType 135))))))))))))))
                              ((StructField
                                ((Reference (res (StructType 138))) slice
                                 (StructType 7)))
                               (StructField
                                ((Reference (res (StructType 138))) value
                                 (StructType 24))))
                              true))))))
                        (if_else
                         ((Expr
                           (Primitive
                            (Prim (name throw) (exprs ((Value (Integer 0))))
                             (out_ty VoidType))))))))))))))))))))))
        (TestUnion (Value (Type (UnionType 135))))))
      (structs
       ((138
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (UnionType 135))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (UnionType 135)) (s (StructType 7))))
                  (function_returns (StructType 138))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 138)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (UnionType 135)))))))))))))))
            (uty_impls ()) (uty_id 138) (uty_base_id -500)))))))
      (unions
       ((135
         ((union_attributes ())
          (cases
           (((StructType 24) (Discriminator (discr 1)))
            ((StructType 35) (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 136)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 35))))
                     (function_returns (UnionType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 35))) 135))))))))))
              ((impl_interface 137)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 24))))
                     (function_returns (UnionType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 24))) 135))))))))))))
            (uty_id 135) (uty_base_id 134)))))))
      (interfaces
       ((137
         ((interface_methods
           ((from
             ((function_params ((from (StructType 24))))
              (function_returns SelfType)))))))
        (136
         ((interface_methods
           ((from
             ((function_params ((from (StructType 35))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 35) (StructType 24))) (un_sig_methods ())
          (un_sig_base_id 134)))))
      (attr_executors <opaque>))) |}]
