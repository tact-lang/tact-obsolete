open Shared.Disabled
open Shared.Disabled.Lang

let add_bin_op_intf p =
  let bl = Tact.Located.Disabled.builtin_located in
  let intf =
    { interface_attributes = [];
      interface_methods =
        [ ( "op",
            bl
              { function_attributes = [];
                function_is_type = false;
                function_params =
                  [(bl "left", IntegerType); (bl "right", IntegerType)];
                function_returns = IntegerType } ) ] }
  in
  let intf_ty =
    Value (Type (Lang.Program.insert_interface_with_id p (-10) intf))
  in
  {p with bindings = (bl "BinOp", bl intf_ty) :: p.bindings}

let%expect_test "program returns" =
  let sources =
    [{| 1 |}; {| return 1;|}; {| fn x() { 1 } x() |}; {| struct T {} |}]
  in
  List.iter ~f:pp_compile sources ;
  [%expect
    {|
    (Ok
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>)))
    (Ok
                                  ((bindings ()) (structs ())
                                   (type_counter <opaque>)
                                   (memoized_fcalls <opaque>)
                                   (struct_signs (0 ())) (union_signs (0 ()))
                                   (attr_executors <opaque>)))
    (Ok
                                                               ((bindings
                                                                 ((x
                                                                   (Value
                                                                    (Function
                                                                     ((function_signature
                                                                       ((function_params
                                                                        ())
                                                                        (function_returns
                                                                        IntegerType)))
                                                                      (function_impl
                                                                       (Fn
                                                                        (Return
                                                                        (Value
                                                                        (Integer
                                                                        1)))))))))))
                                                                (structs ())
                                                                (type_counter
                                                                 <opaque>)
                                                                (memoized_fcalls
                                                                 <opaque>)
                                                                (struct_signs
                                                                 (0 ()))
                                                                (union_signs
                                                                 (0 ()))
                                                                (attr_executors
                                                                 <opaque>)))

    (Ok
     ((bindings ((T (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 132)
          (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "scope resolution" =
  let source = {|
    let T = Int[257];
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 132))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "binding resolution" =
  let source = {|
    let T = Int[257];
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 132))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = NotInt256;
  |} in
  pp_compile source ;
  [%expect
    {|
    (((UnresolvedIdentifier NotInt256))
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int[257];
    let B = A;
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((B (Value (Type (StructType 132)))) (A (Value (Type (StructType 132))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int[257] }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 135))))))
      (structs
       ((135
         ((struct_fields ((t ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))
        (133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((t (Value (Type (StructType 132))))))
          (st_sig_methods ()) (st_sig_base_id 134) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Tact function evaluation" =
  let source =
    {|
    fn test(i: Int[257]) -> Int[257] {
      i
    }
    let a = test(test(Int[257].new(1)));
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a
         (Value
          (Struct
           ((Value (Type (StructType 132))) ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 132))))
              (function_returns (StructType 132))))
            (function_impl (Fn (Return (Reference (i (StructType 132))))))))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
       val a: Int[257]
       val b: Bool
  };
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((MyType (Value (Type (StructType 135))))))
      (structs
       ((135
         ((struct_fields
           ((a ((field_type (StructType 132)))) (b ((field_type BoolType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))
        (133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (Value (Type (StructType 132))))
            (b (ResolvedReference (Bool <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 134) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      val a: Int[257]
      val a: Bool
  };
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (((DuplicateField
       (a
        ((mk_struct_fields
          ((a (Value (Type (StructType 132))))
           (a (ResolvedReference (Bool <opaque>)))))
         (mk_struct_details
          ((mk_methods ()) (mk_impls ()) (mk_id 134) (mk_sig 54)
           (mk_span <opaque>)))))))
     ((bindings ((MyType (Value (Type (StructType 135))))))
      (structs
       ((135
         ((struct_fields
           ((a ((field_type (StructType 132)))) (a ((field_type BoolType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))
        (133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (Value (Type (StructType 132))))
            (a (ResolvedReference (Bool <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 134) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T[A: Type] { val a: A }
      let TA = T[Int[257]];
   |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((TA (Value (Type (StructType 135))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((A (TypeN 0))))
              (function_returns (StructSig 54))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((a (Reference (A (TypeN 0))))))
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 132) (mk_sig 54)
                   (mk_span <opaque>))))))))))))))
      (structs
       ((135
         ((struct_fields ((a ((field_type (StructType 133))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 132)))))
        (134
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 133))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 133)) (s (StructType 7))))
                  (function_returns (StructType 134))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 134)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 133)))))))))))))))
            (uty_impls ()) (uty_id 134) (uty_base_id -500)))))
        (133
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 134))))
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
                       ((Value (Type (StructType 134)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 133)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 133))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 133))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 133)) (other (StructType 133))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 133))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 133))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 133)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 133))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 134))))
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
                          ((Value (Type (StructType 134)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 133)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 133))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 133)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 133) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((a (Reference (A (TypeN 0)))))) (st_sig_methods ())
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Integer 1)))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl (Fn (Return (Value (Integer 1)))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "scoping that `let` introduces in code" =
  let source =
    {|
    fn f(i: Int[257]) {
      let a = i;
      a
    }
    let b = f(Int[257].new(1));
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b
         (Value
          (Struct
           ((Value (Type (StructType 132))) ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 132))))
              (function_returns (StructType 132))))
            (function_impl
             (Fn
              (Block
               ((Let ((a (Reference (i (StructType 132))))))
                (Return (Reference (a (StructType 132))))))))))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "reference in function bodies" =
  let source =
    {|
      fn op(i: Int[257], i_: Int[257]) {
        i
      }

      fn f(x: Int[257]) {
        let a = op(x, x);
        let b = op(a, a);
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((f
         (Value
          (Function
           ((function_signature
             ((function_params ((x (StructType 132))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((a
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (x (StructType 132)))
                      (Reference (x (StructType 132))))
                     false)))))
                (Let
                 ((b
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (a (StructType 132)))
                      (Reference (a (StructType 132))))
                     false)))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 132)) (i_ (StructType 132))))
              (function_returns (StructType 132))))
            (function_impl (Fn (Return (Reference (i (StructType 132))))))))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "resolving a reference from inside a function" =
  let source =
    {|
      let i = 1;
      fn f() {
        i
      }
      let x = f();
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((x (Value (Integer 1)))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl (Fn (Return (ResolvedReference (i <opaque>)))))))))
        (i (Value (Integer 1)))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "struct method access" =
  let source =
    {|
      struct Foo {
        fn bar(self: Self, i: Integer) {
           i
        }
      }
      let foo = Foo {};
      let res = foo.bar(1);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1)))
        (foo (Value (Struct ((Value (Type (StructType 133))) ()))))
        (Foo (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 133)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 54)))))
                (i IntegerType)))
              (function_returns IntegerType)))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "struct type method access" =
  let source =
    {|
      struct Foo {
        fn bar(i: Integer) {
           i
        }
      }
      let res = Foo.bar(1);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1))) (Foo (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params ((i IntegerType))) (function_returns IntegerType)))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Self type resolution in methods" =
  let source =
    {|
      struct Foo {
        fn bar(self: Self) -> Self {
           self
        }
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((Foo (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 133))))
                  (function_returns (StructType 133))))
                (function_impl (Fn (Return (Reference (self (StructType 133))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 54)))))))
              (function_returns (ExprType (Reference (Self (StructSig 54)))))))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "union method access" =
  let source =
    {|
      union Foo {
        case Bool
        fn bar(self: Self, i: Integer) {
          i
        }
      }
      fn make_foo(foo: Foo) -> Foo {
        foo
      }
      let foo = make_foo(true);
      let res = foo.bar(1);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 133))))
        (make_foo
         (Value
          (Function
           ((function_signature
             ((function_params ((foo (UnionType 133))))
              (function_returns (UnionType 133))))
            (function_impl (Fn (Return (Reference (foo (UnionType 133))))))))))
        (Foo (Value (Type (UnionType 133))))))
      (structs ())
      (unions
       ((133
         ((union_attributes ()) (cases ((BoolType (Discriminator (discr 0)))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (UnionType 133)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 134)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 133))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (interfaces
       ((134
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (BoolType))
          (un_sig_methods
           ((bar
             ((function_params
               ((self (ExprType (Reference (Self (UnionSig 6)))))
                (i IntegerType)))
              (function_returns IntegerType)))))
          (un_sig_base_id 132)))))
      (attr_executors <opaque>))) |}]

let%expect_test "union type method access" =
  let source =
    {|
      union Foo {
        case Bool
        fn bar(i: Integer) {
           i
        }
      }
      fn make_foo(foo: Foo) -> Foo {
        foo
      }
      let res = Foo.bar(1);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1)))
        (make_foo
         (Value
          (Function
           ((function_signature
             ((function_params ((foo (UnionType 133))))
              (function_returns (UnionType 133))))
            (function_impl (Fn (Return (Reference (foo (UnionType 133))))))))))
        (Foo (Value (Type (UnionType 133))))))
      (structs ())
      (unions
       ((133
         ((union_attributes ()) (cases ((BoolType (Discriminator (discr 0)))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 134)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 133))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (interfaces
       ((134
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (BoolType))
          (un_sig_methods
           ((bar
             ((function_params ((i IntegerType))) (function_returns IntegerType)))))
          (un_sig_base_id 132)))))
      (attr_executors <opaque>))) |}]

let%expect_test "struct instantiation" =
  let source =
    {|
    struct T {
      val a: Integer
      val b: Integer
    }

    let t = T{a: 1, b: 2};
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((t
         (Value
          (Struct
           ((Value (Type (StructType 133)))
            ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (ResolvedReference (Integer <opaque>)))
            (b (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int[32]) -> Int[64] { return i; }
  |} in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((StructType 116) (StructType 41) <opaque>)))
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 41))))
              (function_returns (StructType 116))))
            (function_impl (Fn (Return (Reference (i (StructType 41))))))))))))
      (structs ())
      (interfaces
       ((132
         ((interface_methods
           ((from
             ((function_params ((from (StructType 41))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "type inference" =
  let source = {|
      fn foo(i: Integer) { return i; }
    |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((i IntegerType))) (function_returns IntegerType)))
            (function_impl (Fn (Return (Reference (i IntegerType)))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "scope doesn't leak bindings" =
  let source = {|
    {
     let a = 1;
    }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>))) |}]

let%expect_test "compile-time if/then/else continues interpretation after \
                 condition check (bug fix)" =
  let source =
    {|
    fn T() {
      if (false) {
        return 1;
      }
      return 2;
    }

    let a = T();
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Integer 2)))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((If
                 ((if_condition (Value (Bool false)))
                  (if_then (Block ((Return (Value (Integer 1)))))) (if_else ())))
                (Return (Value (Integer 2)))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>)))
|}]

let%expect_test "compile-time if/then/else" =
  let source =
    {|
    fn test() {
      true
    }

    fn T() {
      if (test()) {
        return 1;
      } else {
        return 2;
      }
    }

    let a = T();
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Integer 1)))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl
             (Fn
              (If
               ((if_condition (Value (Bool true)))
                (if_then (Block ((Return (Value (Integer 1))))))
                (if_else ((Block ((Return (Value (Integer 2)))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns BoolType)))
            (function_impl (Fn (Return (Value (Bool true)))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "type check error" =
  let source =
    {|
      {
        fn foo(x: Int[99]) { return x; }

        let a = foo(Int[10].new(1));
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((StructType 132) (StructType 134) <opaque>)))
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((x (StructType 132))))
              (function_returns (StructType 132))))
            (function_impl (Fn (Return (Reference (x (StructType 132))))))))))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 10)))
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
                      (Value (Integer 10)))
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
                         (Value (Integer 10)))
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
                            ((Reference (s (StructType 7))) (Value (Integer 10)))
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
              ((impl_interface 17)
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
            (uty_id 134) (uty_base_id 16)))))
        (133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 7))) (Value (Integer 99)))
                         false)))))
                    (DestructuringLet
                     ((destructuring_let ((slice slice) (value value)))
                      (destructuring_let_expr (Reference (res (StructType 5))))
                      (destructuring_let_rest false)))
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 99)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 99)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
                   (function_impl
                    (Fn
                     (Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 7))) (Value (Integer 99)))
                            false)))))
                       (DestructuringLet
                        ((destructuring_let ((slice slice) (value value)))
                         (destructuring_let_expr
                          (Reference (res (StructType 5))))
                         (destructuring_let_rest false)))
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (interfaces
       ((136
         ((interface_methods
           ((from
             ((function_params ((from (StructType 134))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "implement interface op" =
  let source =
    {|
      struct Left {
        impl BinOp {
          fn op(left: Integer, right: Integer) -> Integer { left }
        }
      }
      let one = Left.op(1, 2);
    |}
  in
  pp_compile source ~prev_program:(add_bin_op_intf (Lang.default_program ())) ;
  [%expect
    {|
    (Ok
     ((bindings
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((op
               ((function_signature
                 ((function_params ((left IntegerType) (right IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (left IntegerType)))))))))
            (uty_impls
             (((impl_interface -10)
               (impl_methods
                ((op
                  ((function_signature
                    ((function_params ((left IntegerType) (right IntegerType)))
                     (function_returns IntegerType)))
                   (function_impl (Fn (Return (Reference (left IntegerType))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((op
             ((function_params ((left IntegerType) (right IntegerType)))
              (function_returns IntegerType)))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "implement interface op" =
  let source =
    {|
      interface Make {
        fn new() -> Self
      }
      struct Empty {
        impl Make {
          fn new() -> Self { Self{} }
        }
      }
      let empty = Empty.new();
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((empty (Value (Struct ((Value (Type (StructType 134))) ()))))
        (Empty (Value (Type (StructType 134))))
        (Make (Value (Type (InterfaceType 132))))))
      (structs
       ((134
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ()) (function_returns (StructType 134))))
                (function_impl
                 (Fn
                  (Return (Value (Struct ((Value (Type (StructType 134))) ()))))))))))
            (uty_impls
             (((impl_interface 132)
               (impl_methods
                ((new
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (Value (Struct ((Value (Type (StructType 134))) ())))))))))))))
            (uty_id 134) (uty_base_id 133)))))))
      (interfaces
       ((132
         ((interface_methods
           ((new ((function_params ()) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((new
             ((function_params ())
              (function_returns (ExprType (Reference (Self (StructSig 54)))))))))
          (st_sig_base_id 133) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "serializer inner struct" =
  let source =
    {|
      struct Inner { val x: Int[32] }
      struct Outer { val y: Int[32] val z: Inner }
      let serialize_outer = serializer[Outer];
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((serialize_outer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 135)) (b (StructType 3))))
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
                           ((self (StructType 41)) (builder (StructType 3))))
                          (function_returns (StructType 3))))
                        (function_impl
                         (Fn
                          (Return
                           (FunctionCall
                            ((ResolvedReference (serialize_int <opaque>))
                             ((Reference (builder (StructType 3)))
                              (StructField
                               ((Reference (self (StructType 41))) value
                                IntegerType))
                              (Value (Integer 32)))
                             false))))))))
                     ((StructField
                       ((Reference (self (StructType 135))) y (StructType 41)))
                      (Reference (b (StructType 3))))
                     false)))))
                (Return (Reference (b (StructType 3))))))))))))
        (Outer (Value (Type (StructType 135))))
        (Inner (Value (Type (StructType 133))))))
      (structs
       ((135
         ((struct_fields
           ((y ((field_type (StructType 41))))
            (z ((field_type (StructType 133))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))
        (133
         ((struct_fields ((x ((field_type (StructType 41))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (2
        (((st_sig_fields
           ((y (Value (Type (StructType 41))))
            (z (ResolvedReference (Inner <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 134) (st_sig_id 55))
         ((st_sig_fields ((x (Value (Type (StructType 41))))))
          (st_sig_methods ()) (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "reference resolving in inner functions" =
  let source =
    {|
      fn foo(X: Type) {
        fn(x: X) -> X { x }
      }
      let one = foo(Integer)(1);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((one (Value (Integer 1)))
        (foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0))))
              (function_returns
               (FunctionType
                ((function_params ((x (ExprType (Reference (X (TypeN 0)))))))
                 (function_returns (ExprType (Reference (X (TypeN 0))))))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params ((x (ExprType (Reference (X (TypeN 0)))))))
                   (function_returns (ExprType (Reference (X (TypeN 0)))))))
                 (function_impl
                  (Fn
                   (Return (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "dependent types" =
  let source =
    {|
      fn identity(X: Type) {
        let f = fn(x: X) -> X { x };
        f
      }
      fn test(Y: Type) {
        identity(Y)
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
             ((function_params ((Y (TypeN 0))))
              (function_returns
               (FunctionType
                ((function_params ((x (ExprType (Reference (Y (TypeN 0)))))))
                 (function_returns (ExprType (Reference (Y (TypeN 0))))))))))
            (function_impl
             (Fn
              (Return
               (FunctionCall
                ((ResolvedReference (identity <opaque>))
                 ((Reference (Y (TypeN 0)))) false)))))))))
        (identity
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0))))
              (function_returns
               (FunctionType
                ((function_params ((x (ExprType (Reference (X (TypeN 0)))))))
                 (function_returns (ExprType (Reference (X (TypeN 0))))))))))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((f
                   (MkFunction
                    ((function_signature
                      ((function_params
                        ((x (ExprType (Reference (X (TypeN 0)))))))
                       (function_returns (ExprType (Reference (X (TypeN 0)))))))
                     (function_impl
                      (Fn
                       (Return
                        (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))
                (Return
                 (Reference
                  (f
                   (FunctionType
                    ((function_params ((x (ExprType (Reference (X (TypeN 0)))))))
                     (function_returns (ExprType (Reference (X (TypeN 0))))))))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "TypeN" =
  let source =
    {|
      fn id(X: Type) { X }
      let must_fail = id(Type);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((TypeN 0) (TypeN 1) <opaque>)))
     ((bindings
       ((id
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
            (function_impl (Fn (Return (Reference (X (TypeN 0))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "union variants constructing" =
  let source =
    {|
      union Uni {
        case Integer
        case Int[32]
      }
      fn test(value: Uni) -> Uni {
        value
      }
      let a = test(10);
      let b = test(Int[32].new(1));
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b
         (Value
          (UnionVariant
           ((Struct
             ((Value (Type (StructType 41))) ((value (Value (Integer 1))))))
            133))))
        (a (Value (UnionVariant ((Integer 10) 133))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 133))))
              (function_returns (UnionType 133))))
            (function_impl (Fn (Return (Reference (value (UnionType 133))))))))))
        (Uni (Value (Type (UnionType 133))))))
      (structs ())
      (unions
       ((133
         ((union_attributes ())
          (cases
           (((StructType 41) (Discriminator (discr 1)))
            (IntegerType (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 133))))))))))
              ((impl_interface 134)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 41))))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 41))) 133))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (interfaces
       ((134
         ((interface_methods
           ((from
             ((function_params ((from (StructType 41))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (IntegerType (StructType 41))) (un_sig_methods ())
          (un_sig_base_id 132)))))
      (attr_executors <opaque>))) |}]

let%expect_test "unions duplicate variant" =
  let source =
    {|
      union Test[T: Type] {
        case Integer
        case T
      }
      let a = Test[builtin_Builder]; // should be OK
      let b = Test[Integer]; // should fail
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((DuplicateVariant (IntegerType <opaque>)))
     ((bindings
       ((b (Value (Type (UnionType 135)))) (a (Value (Type (UnionType 133))))
        (Test
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((T (TypeN 0))))
              (function_returns (UnionSig 6))))
            (function_impl
             (Fn
              (Return
               (MkUnionDef
                ((mk_cases
                  (((ResolvedReference (Integer <opaque>)) ())
                   ((Reference (T (TypeN 0))) ())))
                 (mk_union_details
                  ((mk_methods ())
                   (mk_impls
                    (((mk_impl_interface (Value (Type (InterfaceType 17))))
                      (mk_impl_methods
                       ((from
                         (Value
                          (Function
                           ((function_signature
                             ((function_params ((v IntegerType)))
                              (function_returns (UnionType 132))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference (v IntegerType)) 132))))))))))))
                     ((mk_impl_interface
                       (FunctionCall
                        ((Value
                          (Function
                           ((function_signature
                             ((function_is_type)
                              (function_params ((T (TypeN 0))))
                              (function_returns HoleType)))
                            (function_impl (BuiltinFn (<fun> <opaque>))))))
                         ((Value (Type (ExprType (Reference (T (TypeN 0)))))))
                         false)))
                      (mk_impl_methods
                       ((from
                         (Value
                          (Function
                           ((function_signature
                             ((function_params
                               ((v (ExprType (Reference (T (TypeN 0)))))))
                              (function_returns (UnionType 132))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference
                                  (v (ExprType (Reference (T (TypeN 0))))))
                                 132))))))))))))))
                   (mk_id 132) (mk_sig 6) (mk_span <opaque>))))))))))))))
      (structs ())
      (unions
       ((135
         ((union_attributes ()) (cases ((IntegerType (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 135))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 135))))))))))))
            (uty_id 135) (uty_base_id 132)))))
        (133
         ((union_attributes ())
          (cases
           (((BuiltinType Builder) (Discriminator (discr 1)))
            (IntegerType (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 133))))))))))
              ((impl_interface 134)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (BuiltinType Builder))))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant
                       ((Reference (v (BuiltinType Builder))) 133))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (interfaces
       ((134
         ((interface_methods
           ((from
             ((function_params ((from (BuiltinType Builder))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (IntegerType (ExprType (Reference (T (TypeN 0))))))
          (un_sig_methods ()) (un_sig_base_id 132)))))
      (attr_executors <opaque>))) |}]

let%expect_test "unions" =
  let source =
    {|
      union Test {
        case Int[257]
        case Int[64]
        fn id(self: Self) -> Self {
          self
        }
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((Test (Value (Type (UnionType 135))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (unions
       ((135
         ((union_attributes ())
          (cases
           (((StructType 116) (Discriminator (discr 1)))
            ((StructType 132) (Discriminator (discr 0)))))
          (union_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (UnionType 135))))
                  (function_returns (UnionType 135))))
                (function_impl (Fn (Return (Reference (self (UnionType 135))))))))))
            (uty_impls
             (((impl_interface 136)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 132))))
                     (function_returns (UnionType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 132))) 135))))))))))
              ((impl_interface 137)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 116))))
                     (function_returns (UnionType 134))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 116))) 135))))))))))))
            (uty_id 135) (uty_base_id 134)))))))
      (interfaces
       ((137
         ((interface_methods
           ((from
             ((function_params ((from (StructType 116))))
              (function_returns SelfType)))))))
        (136
         ((interface_methods
           ((from
             ((function_params ((from (StructType 132))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 132) (StructType 116)))
          (un_sig_methods
           ((id
             ((function_params
               ((self (ExprType (Reference (Self (UnionSig 6)))))))
              (function_returns (ExprType (Reference (Self (UnionSig 6)))))))))
          (un_sig_base_id 134)))))
      (attr_executors <opaque>))) |}]

let%expect_test "methods monomorphization" =
  let source =
    {|
      fn Foo(X: Type) -> Type {
        struct {
          fn id(self: Self, x: X) -> X { x }
        }
      }
      let foo = Foo(Integer) {};
      let x = foo.id(10);

      struct Empty {}
      let foo_empty = Foo(Empty) {};
      let y = foo_empty.id(Empty{});
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((y (Value (Struct ((Value (Type (StructType 135))) ()))))
        (foo_empty (Value (Struct ((Value (Type (StructType 136))) ()))))
        (Empty (Value (Type (StructType 135)))) (x (Value (Integer 10)))
        (foo (Value (Struct ((Value (Type (StructType 133))) ()))))
        (Foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0))))
              (function_returns (StructSig 54))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ())
                 (mk_struct_details
                  ((mk_methods
                    ((id
                      (MkFunction
                       ((function_signature
                         ((function_params
                           ((self (ExprType (Reference (Self (StructSig 54)))))
                            (x (ExprType (Reference (X (TypeN 0)))))))
                          (function_returns (ExprType (Reference (X (TypeN 0)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))
                   (mk_impls ()) (mk_id 132) (mk_sig 54) (mk_span <opaque>))))))))))))))
      (structs
       ((136
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params
                   ((self (StructType 136)) (x (StructType 135))))
                  (function_returns (StructType 135))))
                (function_impl (Fn (Return (Reference (x (StructType 135))))))))))
            (uty_impls ()) (uty_id 136) (uty_base_id 132)))))
        (135
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 135) (uty_base_id 134)))))
        (133
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (StructType 133)) (x IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (x IntegerType)))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (2
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 134)
          (st_sig_id 55))
         ((st_sig_fields ())
          (st_sig_methods
           ((id
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 54)))))
                (x (ExprType (Reference (X (TypeN 0)))))))
              (function_returns (ExprType (Reference (X (TypeN 0)))))))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "switch statement" =
  let source =
    {|
      union Ints {
        case Int[32]
        case Int[64]
      }
      fn test(i: Ints) -> Integer {
        switch (i) {
          case Int[32] vax => { return 32; }
          case Int[64] vax => { return 64; }
        }
      }
      let must_be_32 = test(Int[32].new(0));
      let must_be_64 = test(Int[64].new(0));
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((must_be_64 (Value (Integer 64))) (must_be_32 (Value (Integer 32)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (UnionType 133))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Switch
               ((switch_condition (Reference (i (UnionType 133))))
                (branches
                 (((branch_ty (StructType 41)) (branch_var vax)
                   (branch_stmt (Block ((Return (Value (Integer 32)))))))
                  ((branch_ty (StructType 116)) (branch_var vax)
                   (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))
        (Ints (Value (Type (UnionType 133))))))
      (structs ())
      (unions
       ((133
         ((union_attributes ())
          (cases
           (((StructType 116) (Discriminator (discr 1)))
            ((StructType 41) (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 134)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 41))))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 41))) 133))))))))))
              ((impl_interface 135)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 116))))
                     (function_returns (UnionType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 116))) 133))))))))))))
            (uty_id 133) (uty_base_id 132)))))))
      (interfaces
       ((135
         ((interface_methods
           ((from
             ((function_params ((from (StructType 116))))
              (function_returns SelfType)))))))
        (134
         ((interface_methods
           ((from
             ((function_params ((from (StructType 41))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 41) (StructType 116))) (un_sig_methods ())
          (un_sig_base_id 132)))))
      (attr_executors <opaque>))) |}]

let%expect_test "partial evaluation of a function" =
  let source =
    {|
      fn left(x: Integer, y: Integer) -> Integer {
        x
      }
      fn test(x: Integer) {
        fn(y: Integer) {
          left(x, y)
        }
      }
      let a = test(10);
      let b = a(20);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b (Value (Integer 10)))
        (a
         (Value
          (Function
           ((function_signature
             ((function_params ((y IntegerType))) (function_returns IntegerType)))
            (function_impl
             (Fn
              (Return
               (FunctionCall
                ((ResolvedReference (left <opaque>))
                 ((Value (Integer 10)) (Reference (y IntegerType))) false)))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((x IntegerType)))
              (function_returns
               (FunctionType
                ((function_params ((y IntegerType)))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params ((y IntegerType)))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Return
                    (FunctionCall
                     ((ResolvedReference (left <opaque>))
                      ((Reference (x IntegerType)) (Reference (y IntegerType)))
                      false))))))))))))))
        (left
         (Value
          (Function
           ((function_signature
             ((function_params ((x IntegerType) (y IntegerType)))
              (function_returns IntegerType)))
            (function_impl (Fn (Return (Reference (x IntegerType)))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "let binding with type" =
  let source = {|
      let a: Int[257] = 1;
      let a: Int[32] = 2;
    |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a
         (Value
          (Struct ((Value (Type (StructType 41))) ((value (Value (Integer 2))))))))
        (a
         (Value
          (Struct
           ((Value (Type (StructType 132))) ((value (Value (Integer 1))))))))))
      (structs
       ((133
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 132))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 132)) (s (StructType 7))))
                  (function_returns (StructType 133))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 133)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 132)))))))))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id -500)))))
        (132
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 133))))
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
                       ((Value (Type (StructType 133)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 132)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value (Reference (i IntegerType))))))))))))
              (add
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (add <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (sub
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (sub <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (mul
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (mul <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (div
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (div <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_and
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_and <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (bit_or
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns (StructType 132))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 132)))
                      ((value
                        (FunctionCall
                         ((ResolvedReference (bit_or <opaque>))
                          ((StructField
                            ((Reference (self (StructType 132))) value
                             IntegerType))
                           (StructField
                            ((Reference (other (StructType 132))) value
                             IntegerType)))
                          false))))))))))))
              (eq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (eq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (neq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (neq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (leq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (leq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (lt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (lt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (geq
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (geq <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))
              (gt
               ((function_signature
                 ((function_params
                   ((self (StructType 132)) (other (StructType 132))))
                  (function_returns BoolType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (gt <opaque>))
                     ((StructField
                       ((Reference (self (StructType 132))) value IntegerType))
                      (StructField
                       ((Reference (other (StructType 132))) value IntegerType)))
                     false)))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 132)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 132))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 133))))
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
                          ((Value (Type (StructType 133)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 132)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 132))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 132)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 132) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "let binding with a non-matching type" =
  let source = {|
      let a: Bool = 1;
    |} in
  pp_compile source ;
  [%expect
    {|
    (((TypeError (BoolType IntegerType <opaque>))
      (TypeError (BoolType IntegerType <opaque>)))
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>))) |}]

let%expect_test "interface constraints" =
  let source =
    {|
    interface Beep {
      fn beep(self: Self) -> Integer
    }
    struct BeeperImpl1 { impl Beep { fn beep(self: Self) -> Integer { 1 } } }
    fn test(T: Beep) {
      fn(t: T) -> Integer {
        return t.beep();
      }
    }
    let concrete_beeper = test(BeeperImpl1);
    let must_be_one = concrete_beeper(BeeperImpl1{});
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((must_be_one (Value (Integer 1)))
        (concrete_beeper
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 134))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Return
               (FunctionCall
                ((Value
                  (Function
                   ((function_signature
                     ((function_params ((self (StructType 134))))
                      (function_returns IntegerType)))
                    (function_impl (Fn (Return (Value (Integer 1))))))))
                 ((Reference (t (StructType 134)))) false)))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 132))))
              (function_returns
               (FunctionType
                ((function_params
                  ((t (ExprType (Reference (T (InterfaceType 132)))))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params
                    ((t (ExprType (Reference (T (InterfaceType 132)))))))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Return
                    (IntfMethodCall
                     ((intf_instance
                       (Value
                        (Type (ExprType (Reference (T (InterfaceType 132)))))))
                      (intf_def 132)
                      (intf_method
                       (beep
                        ((function_params ((self SelfType)))
                         (function_returns IntegerType))))
                      (intf_args
                       ((Reference
                         (t (ExprType (Reference (T (InterfaceType 132))))))))
                      (intf_loc <opaque>)))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 134))))
        (Beep (Value (Type (InterfaceType 132))))))
      (structs
       ((134
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((beep
               ((function_signature
                 ((function_params ((self (StructType 134))))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Value (Integer 1)))))))))
            (uty_impls
             (((impl_interface 132)
               (impl_methods
                ((beep
                  ((function_signature
                    ((function_params ((self (StructType 134))))
                     (function_returns IntegerType)))
                   (function_impl (Fn (Return (Value (Integer 1))))))))))))
            (uty_id 134) (uty_base_id 133)))))))
      (interfaces
       ((132
         ((interface_methods
           ((beep
             ((function_params ((self SelfType))) (function_returns IntegerType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((beep
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 54)))))))
              (function_returns IntegerType)))))
          (st_sig_base_id 133) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "destructuring let" =
  let source =
    {|
      struct T {
         val x: Integer
         val y: Integer
         val z: Integer
      }
      fn test(t: T) -> Integer {
        let {x, y as y2, z} = t;
        y2
      }

      let x = test(T{x: 1, y: 2, z: 3});
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((x (Value (Integer 2)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 133))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((x x) (y y2) (z z)))
                  (destructuring_let_expr (Reference (t (StructType 133))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "destructuring let with missing fields" =
  let source =
    {|
      struct T {
         val x: Integer
         val y: Integer
         val z: Integer
      }
      fn test(t: T) -> Integer {
        let {y as y2} = t;
        y2
      }
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (((MissingField ((StructType 133) x <opaque>))
      (MissingField ((StructType 133) z <opaque>)))
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 133))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 133))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "destructuring let with missing fields ignored" =
  let source =
    {|
      struct T {
         val x: Integer
         val y: Integer
         val z: Integer
      }
      fn test(t: T) -> Integer {
        let {y as y2, ..} = t;
        y2
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
             ((function_params ((t (StructType 133))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 133))))
                  (destructuring_let_rest true)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 133))))))
      (structs
       ((133
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "type that does not implement interface passed to the \
                 constrained argument" =
  let source =
    {|
    interface Intf {}
    fn ExpectedIntf(T: Intf) {}
    struct Foo {}
    let a = ExpectedIntf(Foo);
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((InterfaceType 132) (Type0 (StructType 134)) <opaque>)))
     ((bindings
       ((Foo (Value (Type (StructType 134))))
        (ExpectedIntf
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 132))))
              (function_returns HoleType)))
            (function_impl (Fn (Block ())))))))
        (Intf (Value (Type (InterfaceType 132))))))
      (structs
       ((134
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 134) (uty_base_id 133)))))))
      (interfaces ((132 ((interface_methods ()))))) (type_counter <opaque>)
      (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 133)
          (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "struct signatures" =
  let source =
    {|
       struct IntN2[bits: Integer] {
         val value: Integer
         fn new(i: Integer) -> Self {
           Self { value: i }
         }
       }
       fn extract_value[n: Integer](x: IntN2[n]) -> Integer {
         x.value
       }
       let five = extract_value[10](IntN2[10].new(5));
       let zero = extract_value[20](IntN2[20].new(0));
     |}
  in
  pp_compile source ;
  [%expect
    {|
    (((FieldNotFoundF value) (UnresolvedIdentifier extract_value)
      (UnresolvedIdentifier extract_value))
     ((bindings
       ((IntN2
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((bits IntegerType)))
              (function_returns (StructSig 54))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields
                  ((value (ResolvedReference (Integer <opaque>)))))
                 (mk_struct_details
                  ((mk_methods
                    ((new
                      (MkFunction
                       ((function_signature
                         ((function_params ((i IntegerType)))
                          (function_returns
                           (ExprType (Reference (Self (StructSig 54)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Value
                            (Struct
                             ((Reference (Self (StructSig 54)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (mk_impls ()) (mk_id 132) (mk_sig 54) (mk_span <opaque>))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((value (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods
           ((new
             ((function_params ((i IntegerType)))
              (function_returns (ExprType (Reference (Self (StructSig 54)))))))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Deserilize intf with constraints" =
  let source =
    {|
      struct Container[X: Type] { val x: X }
      interface Deserialize2 {
        fn deserialize() -> Container[Self]
      }
      fn test(Y: Deserialize2) -> Y {
        let v = Y.deserialize();
        v.x
      }

      struct Empty { 
        impl Deserialize2 { 
          fn deserialize() -> Container[Self] { 
            Container[Self] { x: Self {} }
          } 
        }
      }
      let a = test(Empty);
    |}
  in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (((FieldNotFoundF x) (UnresolvedIdentifier test))
     ((bindings
       ((Empty
         (MkStructDef
          ((mk_struct_fields ())
           (mk_struct_details
            ((mk_methods
              ((deserialize
                (MkFunction
                 ((function_signature
                   ((function_params ())
                    (function_returns
                     (TypeCall (func (ResolvedReference (Container <opaque>)))
                      (args ((Reference (Self (StructSig 2)))))))))
                  (function_impl
                   (Fn
                    (Return
                     (Value
                      (Struct
                       ((FunctionCall
                         ((ResolvedReference (Container <opaque>))
                          ((Reference (Self (StructSig 2)))) true))
                        ((x
                          (Value (Struct ((Reference (Self (StructSig 2))) ()))))))))))))))))
             (mk_impls
              (((mk_impl_interface (ResolvedReference (Deserialize2 <opaque>)))
                (mk_impl_methods
                 ((deserialize
                   (MkFunction
                    ((function_signature
                      ((function_params ())
                       (function_returns
                        (TypeCall (func (ResolvedReference (Container <opaque>)))
                         (args ((Reference (Self (StructSig 2)))))))))
                     (function_impl
                      (Fn
                       (Return
                        (Value
                         (Struct
                          ((FunctionCall
                            ((ResolvedReference (Container <opaque>))
                             ((Reference (Self (StructSig 2)))) true))
                           ((x
                             (Value
                              (Struct ((Reference (Self (StructSig 2))) ())))))))))))))))))))
             (mk_id 2) (mk_sig 2) (mk_span <opaque>))))))
        (Deserialize2 (Value (Type (InterfaceType 1))))
        (Container
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((X (TypeN 0))))
              (function_returns (StructSig 1))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((x (Reference (X (TypeN 0))))))
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 0) (mk_sig 1)
                   (mk_span <opaque>))))))))))))))
      (structs ())
      (interfaces
       ((1
         ((interface_methods
           ((deserialize
             ((function_params ())
              (function_returns
               (TypeCall
                (func
                 (Value
                  (Function
                   ((function_signature
                     ((function_is_type) (function_params ((X (TypeN 0))))
                      (function_returns (StructSig 1))))
                    (function_impl
                     (Fn
                      (Return
                       (MkStructDef
                        ((mk_struct_fields ((x (Reference (X (TypeN 0))))))
                         (mk_struct_details
                          ((mk_methods ()) (mk_impls ()) (mk_id 0) (mk_sig 1)
                           (mk_span <opaque>))))))))))))
                (args ((ResolvedReference (Self <opaque>))))))))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (3
        (((st_sig_fields ((x (Reference (Self (StructSig 2))))))
          (st_sig_methods ()) (st_sig_base_id 0) (st_sig_id 3))
         ((st_sig_fields ())
          (st_sig_methods
           ((deserialize
             ((function_params ())
              (function_returns
               (TypeCall (func (ResolvedReference (Container <opaque>)))
                (args ((Reference (Self (StructSig 2)))))))))))
          (st_sig_base_id 2) (st_sig_id 2))
         ((st_sig_fields ((x (Reference (X (TypeN 0)))))) (st_sig_methods ())
          (st_sig_base_id 0) (st_sig_id 1)))))
      (union_signs (0 ())) (attr_executors <opaque>)))
      |}]

let%expect_test "Interface inner constraints" =
  let source =
    {|
      interface Intf { fn do_stuff(self: Self) -> Integer }
      struct Test[X: Intf] { 
        val x: X
        impl Intf { 
          fn do_stuff(self: Self) { self.x.do_stuff(); } 
        } 
      }
      fn test_intf[X: Intf](value: X) -> Integer {
        let temp = Test[X] { x: value };
        return temp.do_stuff();
      }

      struct IntfImpl { 
        impl Intf { 
          fn do_stuff(self: Self) -> Integer {
            return 1;
          } 
        } 
      }

      let one = test_intf[IntfImpl](IntfImpl {});
    |}
  in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (Ok
     ((bindings
       ((one (Value (Integer 1))) (IntfImpl (Value (Type (StructType 3))))
        (test_intf
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((X (InterfaceType 0))))
              (function_returns
               (FunctionType
                ((function_params
                  ((value (ExprType (Reference (X (InterfaceType 0)))))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params
                    ((value (ExprType (Reference (X (InterfaceType 0)))))))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Block
                    ((Let
                      ((temp
                        (Value
                         (Struct
                          ((FunctionCall
                            ((ResolvedReference (Test <opaque>))
                             ((Reference (X (InterfaceType 0)))) true))
                           ((x
                             (Reference
                              (value
                               (ExprType (Reference (X (InterfaceType 0))))))))))))))
                     (Return
                      (StructSigMethodCall
                       ((st_sig_call_instance
                         (Value
                          (Type
                           (TypeCall (func (ResolvedReference (Test <opaque>)))
                            (args ((Reference (X (InterfaceType 0)))))))))
                        (st_sig_call_def 3)
                        (st_sig_call_method
                         (do_stuff
                          ((function_params
                            ((self
                              (TypeCall
                               (func (ResolvedReference (Test <opaque>)))
                               (args ((Reference (X (InterfaceType 0)))))))))
                           (function_returns HoleType))))
                        (st_sig_call_args
                         ((Reference
                           (temp
                            (TypeCall (func (ResolvedReference (Test <opaque>)))
                             (args ((Reference (X (InterfaceType 0))))))))))
                        (st_sig_call_span <opaque>)
                        (st_sig_call_kind StructSigKind)))))))))))))))))
        (Test
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((X (InterfaceType 0))))
              (function_returns (StructSig 1))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((x (Reference (X (InterfaceType 0))))))
                 (mk_struct_details
                  ((mk_methods
                    ((do_stuff
                      (MkFunction
                       ((function_signature
                         ((function_params
                           ((self (ExprType (Reference (Self (StructSig 1)))))))
                          (function_returns HoleType)))
                        (function_impl
                         (Fn
                          (Return
                           (IntfMethodCall
                            ((intf_instance
                              (Value
                               (Type
                                (ExprType (Reference (X (InterfaceType 0)))))))
                             (intf_def 0)
                             (intf_method
                              (do_stuff
                               ((function_params ((self SelfType)))
                                (function_returns IntegerType))))
                             (intf_args
                              ((StructField
                                ((Reference
                                  (self
                                   (ExprType (Reference (Self (StructSig 1))))))
                                 x (ExprType (Reference (X (InterfaceType 0))))))))
                             (intf_loc <opaque>)))))))))))
                   (mk_impls
                    (((mk_impl_interface (ResolvedReference (Intf <opaque>)))
                      (mk_impl_methods
                       ((do_stuff
                         (MkFunction
                          ((function_signature
                            ((function_params
                              ((self (ExprType (Reference (Self (StructSig 1)))))))
                             (function_returns HoleType)))
                           (function_impl
                            (Fn
                             (Return
                              (IntfMethodCall
                               ((intf_instance
                                 (Value
                                  (Type
                                   (ExprType (Reference (X (InterfaceType 0)))))))
                                (intf_def 0)
                                (intf_method
                                 (do_stuff
                                  ((function_params ((self SelfType)))
                                   (function_returns IntegerType))))
                                (intf_args
                                 ((StructField
                                   ((Reference
                                     (self
                                      (ExprType (Reference (Self (StructSig 1))))))
                                    x
                                    (ExprType (Reference (X (InterfaceType 0))))))))
                                (intf_loc <opaque>))))))))))))))
                   (mk_id 1) (mk_sig 1) (mk_span <opaque>))))))))))))
        (Intf (Value (Type (InterfaceType 0))))))
      (structs
       ((4
         ((struct_fields ((x ((field_type (StructType 3))))))
          (struct_details
           ((uty_methods
             ((do_stuff
               ((function_signature
                 ((function_params ((self (StructType 4))))
                  (function_returns HoleType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((self (StructType 3))))
                          (function_returns IntegerType)))
                        (function_impl (Fn (Return (Value (Integer 1))))))))
                     ((StructField
                       ((Reference (self (StructType 4))) x (StructType 3))))
                     false)))))))))
            (uty_impls
             (((impl_interface 0)
               (impl_methods
                ((do_stuff
                  ((function_signature
                    ((function_params ((self (StructType 4))))
                     (function_returns HoleType)))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params ((self (StructType 3))))
                             (function_returns IntegerType)))
                           (function_impl (Fn (Return (Value (Integer 1))))))))
                        ((StructField
                          ((Reference (self (StructType 4))) x (StructType 3))))
                        false))))))))))))
            (uty_id 4) (uty_base_id 1)))))
        (3
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((do_stuff
               ((function_signature
                 ((function_params ((self (StructType 3))))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Value (Integer 1)))))))))
            (uty_impls
             (((impl_interface 0)
               (impl_methods
                ((do_stuff
                  ((function_signature
                    ((function_params ((self (StructType 3))))
                     (function_returns IntegerType)))
                   (function_impl (Fn (Return (Value (Integer 1))))))))))))
            (uty_id 3) (uty_base_id 2)))))))
      (interfaces
       ((0
         ((interface_methods
           ((do_stuff
             ((function_params ((self SelfType))) (function_returns IntegerType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (4
        (((st_sig_fields ())
          (st_sig_methods
           ((do_stuff
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 4)))))))
              (function_returns IntegerType)))))
          (st_sig_base_id 2) (st_sig_id 4))
         ((st_sig_fields ((x (Reference (X (InterfaceType 0))))))
          (st_sig_methods
           ((do_stuff
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 1)))))))
              (function_returns HoleType)))))
          (st_sig_base_id 1) (st_sig_id 3))
         ((st_sig_fields ((x (Reference (X (InterfaceType 0))))))
          (st_sig_methods
           ((do_stuff
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 1)))))))
              (function_returns HoleType)))))
          (st_sig_base_id 1) (st_sig_id 2))
         ((st_sig_fields ((x (Reference (X (InterfaceType 0))))))
          (st_sig_methods
           ((do_stuff
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 1)))))))
              (function_returns HoleType)))))
          (st_sig_base_id 1) (st_sig_id 1)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "compile-time assignment" =
  let source =
    {|
      fn test() { 2 }
      let a = 1;
      a = test();
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Integer 2)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl (Fn (Return (Value (Integer 2)))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "attributes" =
  let source =
    {|
    @attr
    @attr(1)
    @attr(1,2)
    struct T {
      @attr val a: Integer
      @attr fn x() { true }
    }

    @attr
    struct Ta[X: Integer] {}

    let T1 = @attr struct { };

    @attr
    fn x() { }

    let x1 = @attr fn () { };

    @attr
    interface I {
      @attr
      fn x() -> Bool
    }

    @attr
    union U { case Void }

    let U1 = @attr union { case Void };

    struct Ti {
      @attr
      impl I {
        @attr
        fn x() -> Bool { true } 
      }
    }

    /* FIXME: we don't handle enums yet
    @attr
    enum E {
      @attr fn x() { true }
    }

    let E1 = @attr enum { }
    */

 
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((Ti (Value (Type (StructType 144)))) (U1 (Value (Type (UnionType 142))))
        (U (Value (Type (UnionType 139)))) (I (Value (Type (InterfaceType 137))))
        (x1
         (Value
          (Function
           ((function_signature
             ((function_attributes
               (((attribute_ident attr) (attribute_exprs ()))))
              (function_params ()) (function_returns HoleType)))
            (function_impl (Fn (Block ())))))))
        (x
         (Value
          (Function
           ((function_signature
             ((function_attributes
               (((attribute_ident attr) (attribute_exprs ()))))
              (function_params ()) (function_returns HoleType)))
            (function_impl (Fn (Block ())))))))
        (T1 (Value (Type (StructType 136))))
        (Ta
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((X IntegerType)))
              (function_returns (StructSig 55))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_attributes
                  (((attribute_ident attr) (attribute_exprs ()))))
                 (mk_struct_fields ())
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 134) (mk_sig 55)
                   (mk_span <opaque>))))))))))))
        (T (Value (Type (StructType 133))))))
      (structs
       ((144
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((x
               ((function_signature
                 ((function_attributes
                   (((attribute_ident attr) (attribute_exprs ()))))
                  (function_params ()) (function_returns BoolType)))
                (function_impl (Fn (Return (Value (Bool true)))))))))
            (uty_impls
             (((impl_attributes (((attribute_ident attr) (attribute_exprs ()))))
               (impl_interface 137)
               (impl_methods
                ((x
                  ((function_signature
                    ((function_attributes
                      (((attribute_ident attr) (attribute_exprs ()))))
                     (function_params ()) (function_returns BoolType)))
                   (function_impl (Fn (Return (Value (Bool true))))))))))))
            (uty_id 144) (uty_base_id 143)))))
        (136
         ((struct_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 136) (uty_base_id 135)))))
        (133
         ((struct_attributes
           (((attribute_ident attr) (attribute_exprs ()))
            ((attribute_ident attr) (attribute_exprs ((Value (Integer 1)))))
            ((attribute_ident attr)
             (attribute_exprs ((Value (Integer 1)) (Value (Integer 2)))))))
          (struct_fields ((a ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((x
               ((function_signature
                 ((function_attributes
                   (((attribute_ident attr) (attribute_exprs ()))))
                  (function_params ()) (function_returns BoolType)))
                (function_impl (Fn (Return (Value (Bool true)))))))))
            (uty_impls ()) (uty_id 133) (uty_base_id 132)))))))
      (unions
       ((142
         ((union_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (cases (((ExprType (Value Void)) (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 140)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v VoidType)))
                     (function_returns (UnionType 141))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v VoidType)) 142))))))))))))
            (uty_id 142) (uty_base_id 141)))))
        (139
         ((union_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (cases (((ExprType (Value Void)) (Discriminator (discr 0)))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 140)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v VoidType)))
                     (function_returns (UnionType 138))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v VoidType)) 139))))))))))))
            (uty_id 139) (uty_base_id 138)))))))
      (interfaces
       ((140
         ((interface_methods
           ((from
             ((function_params ((from VoidType))) (function_returns SelfType)))))))
        (137
         ((interface_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (interface_methods
           ((x
             ((function_attributes
               (((attribute_ident attr) (attribute_exprs ()))))
              (function_params ()) (function_returns BoolType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (4
        (((st_sig_fields ())
          (st_sig_methods
           ((x
             ((function_attributes
               (((attribute_ident attr) (attribute_exprs ()))))
              (function_params ()) (function_returns BoolType)))))
          (st_sig_base_id 143) (st_sig_id 57))
         ((st_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 135)
          (st_sig_id 56))
         ((st_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 134)
          (st_sig_id 55))
         ((st_sig_attributes
           (((attribute_ident attr) (attribute_exprs ()))
            ((attribute_ident attr) (attribute_exprs ((Value (Integer 1)))))
            ((attribute_ident attr)
             (attribute_exprs ((Value (Integer 1)) (Value (Integer 2)))))))
          (st_sig_fields ((a (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods
           ((x
             ((function_attributes
               (((attribute_ident attr) (attribute_exprs ()))))
              (function_params ()) (function_returns BoolType)))))
          (st_sig_base_id 132) (st_sig_id 54)))))
      (union_signs
       (2
        (((un_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (un_sig_cases ((ExprType (Value Void)))) (un_sig_methods ())
          (un_sig_base_id 141))
         ((un_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (un_sig_cases ((ExprType (Value Void)))) (un_sig_methods ())
          (un_sig_base_id 138)))))
      (attr_executors <opaque>))) |}]

let%expect_test "runtime assignment" =
  let source =
    {|
      fn test(n: Integer) {
        let a = 1;
        a = n;
        a
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
               ((function_params ((n IntegerType))) (function_returns IntegerType)))
              (function_impl
               (Fn
                (Block
                 ((Let ((a (Value (Integer 1)))))
                  (Assignment
                   ((assignment_lvalue (ReferenceLvalue a))
                    (assignment_expr (Reference (n IntegerType)))))
                  (Return (Reference (a IntegerType)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "assignment without initial declaration" =
  let source = {|
      a = 2;
  |} in
  pp_compile source ;
  [%expect
    {|
    (((UnresolvedIdentifier a))
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>))) |}]

let%expect_test "assignment with condition block" =
  let source =
    {|
      fn test() {
        let a = 1;
        if (true) {
          a = 10;
        } else {
          a = 20;
        }
        return a;
      }
    |}
  in
  pp_compile source ~include_std:false ~strip_defaults:true ;
  [%expect
    {|
      (Ok
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ()) (function_returns IntegerType)))
              (function_impl
               (Fn
                (Block
                 ((Let ((a (Value (Integer 1)))))
                  (If
                   ((if_condition (Value (Bool true)))
                    (if_then
                     (Block
                      ((Assignment
                        ((assignment_lvalue (ReferenceLvalue a))
                         (assignment_expr (Value (Integer 10))))))))
                    (if_else
                     ((Block
                       ((Assignment
                         ((assignment_lvalue (ReferenceLvalue a))
                          (assignment_expr (Value (Integer 20)))))))))))
                  (Return (Reference (a IntegerType)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "field assignment OK" =
  let source =
    {|
      struct Test { val field: Integer }
      fn test() {
        let var = Test { field: 10 };
        var.field = 20;
      }
    |}
  in
  pp_compile source ~include_std:false ~strip_defaults:true ;
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
                 ((var
                   (Value
                    (Struct
                     ((Value (Type (StructType 1)))
                      ((field (Value (Integer 10))))))))))
                (Assignment
                 ((assignment_lvalue
                   (FieldAccessLvalue (ref var) (ref_ty (StructType 1))
                    (fields (field))))
                  (assignment_expr (Value (Integer 20)))))))))))))
        (Test (Value (Type (StructType 1))))))
      (structs
       ((1
         ((struct_fields ((field ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 1) (uty_base_id 0)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((field (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 0) (st_sig_id 1)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "field assignment WRONG TYPE" =
  let source =
    {|
      struct Test { val field: Integer }
      fn test() {
        let var = Test { field: 10 };
        var.field = true;
      }
    |}
  in
  pp_compile source ~include_std:false ~strip_defaults:true ;
  [%expect
    {|
    (((TypeError (IntegerType BoolType <opaque>)))
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
                 ((var
                   (Value
                    (Struct
                     ((Value (Type (StructType 1)))
                      ((field (Value (Integer 10))))))))))
                (Assignment
                 ((assignment_lvalue
                   (FieldAccessLvalue (ref var) (ref_ty (StructType 1))
                    (fields (field))))
                  (assignment_expr (Value Void))))))))))))
        (Test (Value (Type (StructType 1))))))
      (structs
       ((1
         ((struct_fields ((field ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 1) (uty_base_id 0)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((field (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 0) (st_sig_id 1)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "methods incrementally added" =
  let source =
    {|
    struct Test {
      fn method1(self: Self) {}
      fn method2(self: Self) { self.method1(); }
    }
    union Test {
      fn method1(self: Self) {}
      fn method2(self: Self) { self.method1(); }
    }
  |}
  in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (Ok
     ((bindings
       ((Test (Value (Type (UnionType 3)))) (Test (Value (Type (StructType 1))))))
      (structs
       ((1
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((method1
               ((function_signature
                 ((function_params ((self (StructType 1))))
                  (function_returns HoleType)))
                (function_impl (Fn (Block ())))))
              (method2
               ((function_signature
                 ((function_params ((self (StructType 1))))
                  (function_returns HoleType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((self (StructType 1))))
                          (function_returns HoleType)))
                        (function_impl (Fn (Block ()))))))
                     ((Reference (self (StructType 1)))) false)))))))))
            (uty_impls ()) (uty_id 1) (uty_base_id 0)))))))
      (unions
       ((3
         ((union_attributes ()) (cases ())
          (union_details
           ((uty_methods
             ((method1
               ((function_signature
                 ((function_params ((self (UnionType 3))))
                  (function_returns HoleType)))
                (function_impl (Fn (Block ())))))
              (method2
               ((function_signature
                 ((function_params ((self (UnionType 3))))
                  (function_returns HoleType)))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params ((self (UnionType 3))))
                          (function_returns HoleType)))
                        (function_impl (Fn (Block ()))))))
                     ((Reference (self (UnionType 3)))) false)))))))))
            (uty_impls ()) (uty_id 3) (uty_base_id 2)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((method2
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 1)))))))
              (function_returns HoleType)))
            (method1
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 1)))))))
              (function_returns HoleType)))))
          (st_sig_base_id 0) (st_sig_id 1)))))
      (union_signs
       (1
        (((un_sig_cases ())
          (un_sig_methods
           ((method2
             ((function_params
               ((self (ExprType (Reference (Self (UnionSig 0)))))))
              (function_returns HoleType)))
            (method1
             ((function_params
               ((self (ExprType (Reference (Self (UnionSig 0)))))))
              (function_returns HoleType)))))
          (un_sig_base_id 2)))))
      (attr_executors <opaque>))) |}]

let%expect_test "Universal Functions" =
  let source = {|
    let three = 1.add(2);
  |} in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (Ok
     ((bindings ((three (Value (Integer 3))))) (structs ())
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "Operators" =
  let source = {|
    let _ = 1 + 2 - 3 * 4 / 5 & 6 | 7;
  |} in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (Ok
     ((bindings ((_ (Value (Integer 2))))) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ())) (union_signs (0 ()))
      (attr_executors <opaque>))) |}]
