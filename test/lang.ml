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
     ((bindings ((T (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 120)
          (st_sig_id 46)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "scope resolution" =
  let source = {|
    let T = Int[257];
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 120))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
     ((bindings ((T (Value (Type (StructType 120))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp_compile source ;
  [%expect
    {|
    (((UnresolvedIdentifier Int256))
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
       ((B (Value (Type (StructType 120)))) (A (Value (Type (StructType 120))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
     ((bindings ((T (Value (Type (StructType 123))))))
      (structs
       ((123
         ((struct_fields ((t ((field_type (StructType 120))))))
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
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((t (Value (Type (StructType 120))))))
          (st_sig_methods ()) (st_sig_base_id 122) (st_sig_id 46)))))
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
           ((Value (Type (StructType 120))) ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 120))))
              (function_returns (StructType 120))))
            (function_impl (Fn (Return (Reference (i (StructType 120))))))))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
     ((bindings ((MyType (Value (Type (StructType 123))))))
      (structs
       ((123
         ((struct_fields
           ((a ((field_type (StructType 120)))) (b ((field_type BoolType)))))
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
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (Value (Type (StructType 120))))
            (b (ResolvedReference (Bool <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 122) (st_sig_id 46)))))
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
          ((a (Value (Type (StructType 120))))
           (a (ResolvedReference (Bool <opaque>)))))
         (mk_struct_details
          ((mk_methods ()) (mk_impls ()) (mk_id 122) (mk_sig 46)
           (mk_span <opaque>)))))))
     ((bindings ((MyType (Value (Type (StructType 123))))))
      (structs
       ((123
         ((struct_fields
           ((a ((field_type (StructType 120)))) (a ((field_type BoolType)))))
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
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (Value (Type (StructType 120))))
            (a (ResolvedReference (Bool <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 122) (st_sig_id 46)))))
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
       ((TA (Value (Type (StructType 123))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((A (TypeN 0))))
              (function_returns (StructSig 46))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((a (Reference (A (TypeN 0))))))
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 120) (mk_sig 46)
                   (mk_span <opaque>))))))))))))))
      (structs
       ((123
         ((struct_fields ((a ((field_type (StructType 121))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 123) (uty_base_id 120)))))
        (122
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 121))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 121)) (s (StructType 7))))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 121)))))))))))))))
            (uty_impls ()) (uty_id 122) (uty_base_id -500)))))
        (121
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 121))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 121)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 122))))
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
                       ((Value (Type (StructType 122)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 121)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 121)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 121))) value IntegerType))
                      (Value (Integer 257)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 121))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 121)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 121)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 121))) value IntegerType))
                         (Value (Integer 257)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 122))))
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
                          ((Value (Type (StructType 122)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 121)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 121))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 121)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 121) (uty_base_id 16)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((a (Reference (A (TypeN 0)))))) (st_sig_methods ())
          (st_sig_base_id 120) (st_sig_id 46)))))
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
           ((Value (Type (StructType 120))) ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 120))))
              (function_returns (StructType 120))))
            (function_impl
             (Fn
              (Block
               ((Let ((a (Reference (i (StructType 120))))))
                (Return (Reference (a (StructType 120))))))))))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
             ((function_params ((x (StructType 120))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((a
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (x (StructType 120)))
                      (Reference (x (StructType 120))))
                     false)))))
                (Let
                 ((b
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (a (StructType 120)))
                      (Reference (a (StructType 120))))
                     false)))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 120)) (i_ (StructType 120))))
              (function_returns (StructType 120))))
            (function_impl (Fn (Return (Reference (i (StructType 120))))))))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
        (foo (Value (Struct ((Value (Type (StructType 121))) ()))))
        (Foo (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 121)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 46)))))
                (i IntegerType)))
              (function_returns IntegerType)))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
       ((res (Value (Integer 1))) (Foo (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params ((i IntegerType))) (function_returns IntegerType)))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
     ((bindings ((Foo (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 121))))
                  (function_returns (StructType 121))))
                (function_impl (Fn (Return (Reference (self (StructType 121))))))))))
            (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((bar
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 46)))))))
              (function_returns (ExprType (Reference (Self (StructSig 46)))))))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
       ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 121))))
        (make_foo
         (Value
          (Function
           ((function_signature
             ((function_params ((foo (UnionType 121))))
              (function_returns (UnionType 121))))
            (function_impl (Fn (Return (Reference (foo (UnionType 121))))))))))
        (Foo (Value (Type (UnionType 121))))))
      (structs ())
      (unions
       ((121
         ((union_attributes ()) (cases ((BoolType (Discriminator 0))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (UnionType 121)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 122)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 121))))))))))))
            (uty_id 121) (uty_base_id 120)))))))
      (interfaces
       ((122
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
               ((self (ExprType (Reference (Self (UnionSig 5)))))
                (i IntegerType)))
              (function_returns IntegerType)))))
          (un_sig_base_id 120)))))
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
             ((function_params ((foo (UnionType 121))))
              (function_returns (UnionType 121))))
            (function_impl (Fn (Return (Reference (foo (UnionType 121))))))))))
        (Foo (Value (Type (UnionType 121))))))
      (structs ())
      (unions
       ((121
         ((union_attributes ()) (cases ((BoolType (Discriminator 0))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 122)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 121))))))))))))
            (uty_id 121) (uty_base_id 120)))))))
      (interfaces
       ((122
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
          (un_sig_base_id 120)))))
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
           ((Value (Type (StructType 121)))
            ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((a (ResolvedReference (Integer <opaque>)))
            (b (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 120) (st_sig_id 46)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int[32]) -> Int[64] { return i; }
  |} in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((StructType 120) (StructType 41) <opaque>)))
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 41))))
              (function_returns (StructType 120))))
            (function_impl (Fn (Return (Reference (i (StructType 41))))))))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 64)))
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
                      (Value (Integer 64)))
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
                         (Value (Integer 64)))
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
                            ((Reference (s (StructType 7))) (Value (Integer 64)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (interfaces
       ((122
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
    (((TypeError ((StructType 120) (StructType 122) <opaque>))
      (ArgumentNumberMismatch (1 1 <opaque>)))
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((x (StructType 120))))
              (function_returns (StructType 120))))
            (function_impl (Fn (Return (Reference (x (StructType 120))))))))))))
      (structs
       ((123
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 122))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 122)) (s (StructType 7))))
                  (function_returns (StructType 123))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 123)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 122)))))))))))))))
            (uty_impls ()) (uty_id 123) (uty_base_id -500)))))
        (122
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 123))))
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
                       ((Value (Type (StructType 123)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 122)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 122)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 122))) value IntegerType))
                      (Value (Integer 10)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 122)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 122))) value IntegerType))
                         (Value (Integer 10)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 123))))
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
                          ((Value (Type (StructType 123)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 122)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 122))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 122)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 122) (uty_base_id 16)))))
        (121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 99)))
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
                      (Value (Integer 99)))
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
                         (Value (Integer 99)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (interfaces
       ((124
         ((interface_methods
           ((from
             ((function_params ((from (StructType 122))))
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
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 121))))))
      (structs
       ((121
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
            (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((op
             ((function_params ((left IntegerType) (right IntegerType)))
              (function_returns IntegerType)))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
       ((empty (Value (Struct ((Value (Type (StructType 122))) ()))))
        (Empty (Value (Type (StructType 122))))
        (Make (Value (Type (InterfaceType 120))))))
      (structs
       ((122
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ()) (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return (Value (Struct ((Value (Type (StructType 122))) ()))))))))))
            (uty_impls
             (((impl_interface 120)
               (impl_methods
                ((new
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 122))))
                   (function_impl
                    (Fn
                     (Return
                      (Value (Struct ((Value (Type (StructType 122))) ())))))))))))))
            (uty_id 122) (uty_base_id 121)))))))
      (interfaces
       ((120
         ((interface_methods
           ((new ((function_params ()) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ())
          (st_sig_methods
           ((new
             ((function_params ())
              (function_returns (ExprType (Reference (Self (StructSig 46)))))))))
          (st_sig_base_id 121) (st_sig_id 46)))))
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
                       ((Reference (self (StructType 123))) y (StructType 41)))
                      (Reference (b (StructType 3))))
                     false)))))
                (Return (Reference (b (StructType 3))))))))))))
        (Outer (Value (Type (StructType 123))))
        (Inner (Value (Type (StructType 121))))))
      (structs
       ((123
         ((struct_fields
           ((y ((field_type (StructType 41))))
            (z ((field_type (StructType 121))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 123) (uty_base_id 122)))))
        (121
         ((struct_fields ((x ((field_type (StructType 41))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (2
        (((st_sig_fields
           ((y (Value (Type (StructType 41))))
            (z (ResolvedReference (Inner <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 122) (st_sig_id 47))
         ((st_sig_fields ((x (Value (Type (StructType 41))))))
          (st_sig_methods ()) (st_sig_base_id 120) (st_sig_id 46)))))
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
    (((TypeError ((TypeN 0) (TypeN 1) <opaque>))
      (ArgumentNumberMismatch (1 1 <opaque>)))
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
            121))))
        (a (Value (UnionVariant ((Integer 10) 121))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 121))))
              (function_returns (UnionType 121))))
            (function_impl (Fn (Return (Reference (value (UnionType 121))))))))))
        (Uni (Value (Type (UnionType 121))))))
      (structs ())
      (unions
       ((121
         ((union_attributes ())
          (cases
           (((StructType 41) (Discriminator 1)) (IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 121))))))))))
              ((impl_interface 122)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 41))))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 41))) 121))))))))))))
            (uty_id 121) (uty_base_id 120)))))))
      (interfaces
       ((122
         ((interface_methods
           ((from
             ((function_params ((from (StructType 41))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (IntegerType (StructType 41))) (un_sig_methods ())
          (un_sig_base_id 120)))))
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
       ((b (Value (Type (UnionType 123)))) (a (Value (Type (UnionType 121))))
        (Test
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((T (TypeN 0))))
              (function_returns (UnionSig 5))))
            (function_impl
             (Fn
              (Return
               (MkUnionDef
                ((mk_cases
                  ((ResolvedReference (Integer <opaque>))
                   (Reference (T (TypeN 0)))))
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
                              (function_returns (UnionType 120))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference (v IntegerType)) 120))))))))))))
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
                              (function_returns (UnionType 120))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference
                                  (v (ExprType (Reference (T (TypeN 0))))))
                                 120))))))))))))))
                   (mk_id 120) (mk_sig 5) (mk_span <opaque>))))))))))))))
      (structs ())
      (unions
       ((123
         ((union_attributes ()) (cases ((IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 123))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 123))))))))))))
            (uty_id 123) (uty_base_id 120)))))
        (121
         ((union_attributes ())
          (cases
           (((BuiltinType Builder) (Discriminator 1))
            (IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v IntegerType)) 121))))))))))
              ((impl_interface 122)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (BuiltinType Builder))))
                     (function_returns (UnionType 120))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant
                       ((Reference (v (BuiltinType Builder))) 121))))))))))))
            (uty_id 121) (uty_base_id 120)))))))
      (interfaces
       ((122
         ((interface_methods
           ((from
             ((function_params ((from (BuiltinType Builder))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases (IntegerType (ExprType (Reference (T (TypeN 0))))))
          (un_sig_methods ()) (un_sig_base_id 120)))))
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
     ((bindings ((Test (Value (Type (UnionType 125))))))
      (structs
       ((123
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 122))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 122)) (s (StructType 7))))
                  (function_returns (StructType 123))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 123)))
                      ((slice (Reference (s (StructType 7))))
                       (value (Reference (v (StructType 122)))))))))))))))
            (uty_impls ()) (uty_id 123) (uty_base_id -500)))))
        (122
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((value (Reference (i IntegerType))))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 123))))
                (function_impl
                 (Fn
                  (Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 7))) (Value (Integer 64)))
                         false)))))
                    (DestructuringLet
                     ((destructuring_let ((slice slice) (value value)))
                      (destructuring_let_expr (Reference (res (StructType 5))))
                      (destructuring_let_rest false)))
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 123)))
                        ((slice (Reference (slice (StructType 7))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 122)))
                             ((value (Reference (value IntegerType)))))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 122)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 122))) value IntegerType))
                      (Value (Integer 64)))
                     false)))))))
              (new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 122))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 122)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 122)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 122))) value IntegerType))
                         (Value (Integer 64)))
                        false))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 123))))
                   (function_impl
                    (Fn
                     (Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 7))) (Value (Integer 64)))
                            false)))))
                       (DestructuringLet
                        ((destructuring_let ((slice slice) (value value)))
                         (destructuring_let_expr
                          (Reference (res (StructType 5))))
                         (destructuring_let_rest false)))
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 123)))
                           ((slice (Reference (slice (StructType 7))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 122)))
                                ((value (Reference (value IntegerType))))))))))))))))))))))
              ((impl_interface 17)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 122))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 122)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 122) (uty_base_id 16)))))
        (121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (unions
       ((125
         ((union_attributes ())
          (cases
           (((StructType 122) (Discriminator 1))
            ((StructType 120) (Discriminator 0))))
          (union_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (UnionType 125))))
                  (function_returns (UnionType 125))))
                (function_impl (Fn (Return (Reference (self (UnionType 125))))))))))
            (uty_impls
             (((impl_interface 126)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 120))))
                     (function_returns (UnionType 124))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 120))) 125))))))))))
              ((impl_interface 127)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 122))))
                     (function_returns (UnionType 124))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 122))) 125))))))))))))
            (uty_id 125) (uty_base_id 124)))))))
      (interfaces
       ((127
         ((interface_methods
           ((from
             ((function_params ((from (StructType 122))))
              (function_returns SelfType)))))))
        (126
         ((interface_methods
           ((from
             ((function_params ((from (StructType 120))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 120) (StructType 122)))
          (un_sig_methods
           ((id
             ((function_params
               ((self (ExprType (Reference (Self (UnionSig 5)))))))
              (function_returns (ExprType (Reference (Self (UnionSig 5)))))))))
          (un_sig_base_id 124)))))
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
       ((y (Value (Struct ((Value (Type (StructType 123))) ()))))
        (foo_empty (Value (Struct ((Value (Type (StructType 124))) ()))))
        (Empty (Value (Type (StructType 123)))) (x (Value (Integer 10)))
        (foo (Value (Struct ((Value (Type (StructType 121))) ()))))
        (Foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0))))
              (function_returns (StructSig 46))))
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
                           ((self (ExprType (Reference (Self (StructSig 46)))))
                            (x (ExprType (Reference (X (TypeN 0)))))))
                          (function_returns (ExprType (Reference (X (TypeN 0)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))
                   (mk_impls ()) (mk_id 120) (mk_sig 46) (mk_span <opaque>))))))))))))))
      (structs
       ((124
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params
                   ((self (StructType 124)) (x (StructType 123))))
                  (function_returns (StructType 123))))
                (function_impl (Fn (Return (Reference (x (StructType 123))))))))))
            (uty_impls ()) (uty_id 124) (uty_base_id 120)))))
        (123
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 123) (uty_base_id 122)))))
        (121
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (StructType 121)) (x IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (x IntegerType)))))))))
            (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (2
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 122)
          (st_sig_id 47))
         ((st_sig_fields ())
          (st_sig_methods
           ((id
             ((function_params
               ((self (ExprType (Reference (Self (StructSig 46)))))
                (x (ExprType (Reference (X (TypeN 0)))))))
              (function_returns (ExprType (Reference (X (TypeN 0)))))))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
             ((function_params ((i (UnionType 123))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Switch
               ((switch_condition (Reference (i (UnionType 123))))
                (branches
                 (((branch_ty (StructType 41)) (branch_var vax)
                   (branch_stmt (Block ((Return (Value (Integer 32)))))))
                  ((branch_ty (StructType 120)) (branch_var vax)
                   (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))
        (Ints (Value (Type (UnionType 123))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 64)))
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
                      (Value (Integer 64)))
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
                         (Value (Integer 64)))
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
                            ((Reference (s (StructType 7))) (Value (Integer 64)))
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
            (uty_id 120) (uty_base_id 16)))))))
      (unions
       ((123
         ((union_attributes ())
          (cases
           (((StructType 120) (Discriminator 1))
            ((StructType 41) (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 124)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 41))))
                     (function_returns (UnionType 122))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 41))) 123))))))))))
              ((impl_interface 125)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 120))))
                     (function_returns (UnionType 122))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 120))) 123))))))))))))
            (uty_id 123) (uty_base_id 122)))))))
      (interfaces
       ((125
         ((interface_methods
           ((from
             ((function_params ((from (StructType 120))))
              (function_returns SelfType)))))))
        (124
         ((interface_methods
           ((from
             ((function_params ((from (StructType 41))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (1
        (((un_sig_cases ((StructType 41) (StructType 120))) (un_sig_methods ())
          (un_sig_base_id 122)))))
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
           ((Value (Type (StructType 120))) ((value (Value (Integer 1))))))))))
      (structs
       ((121
         ((struct_fields
           ((slice ((field_type (StructType 7))))
            (value ((field_type (StructType 120))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((v (StructType 120)) (s (StructType 7))))
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
                         ((Reference (s (StructType 7))) (Value (Integer 257)))
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
                      (Value (Integer 257)))
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
                         (Value (Integer 257)))
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
            (uty_id 120) (uty_base_id 16)))))))
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
      (TypeError (BoolType IntegerType <opaque>))
      (ArgumentNumberMismatch (1 1 <opaque>)))
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
             ((function_params ((t (StructType 122))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Return
               (FunctionCall
                ((Value
                  (Function
                   ((function_signature
                     ((function_params ((self (StructType 122))))
                      (function_returns IntegerType)))
                    (function_impl (Fn (Return (Value (Integer 1))))))))
                 ((Reference (t (StructType 122)))) false)))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 120))))
              (function_returns
               (FunctionType
                ((function_params
                  ((t (ExprType (Reference (T (InterfaceType 120)))))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params
                    ((t (ExprType (Reference (T (InterfaceType 120)))))))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Return
                    (IntfMethodCall
                     ((intf_instance
                       (Value
                        (Type (ExprType (Reference (T (InterfaceType 120)))))))
                      (intf_def 120)
                      (intf_method
                       (beep
                        ((function_params ((self SelfType)))
                         (function_returns IntegerType))))
                      (intf_args
                       ((Reference
                         (t (ExprType (Reference (T (InterfaceType 120))))))))
                      (intf_loc <opaque>)))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 122))))
        (Beep (Value (Type (InterfaceType 120))))))
      (structs
       ((122
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((beep
               ((function_signature
                 ((function_params ((self (StructType 122))))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Value (Integer 1)))))))))
            (uty_impls
             (((impl_interface 120)
               (impl_methods
                ((beep
                  ((function_signature
                    ((function_params ((self (StructType 122))))
                     (function_returns IntegerType)))
                   (function_impl (Fn (Return (Value (Integer 1))))))))))))
            (uty_id 122) (uty_base_id 121)))))))
      (interfaces
       ((120
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
               ((self (ExprType (Reference (Self (StructSig 46)))))))
              (function_returns IntegerType)))))
          (st_sig_base_id 121) (st_sig_id 46)))))
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
             ((function_params ((t (StructType 121))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((x x) (y y2) (z z)))
                  (destructuring_let_expr (Reference (t (StructType 121))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 120) (st_sig_id 46)))))
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
    (((MissingField ((StructType 121) x <opaque>))
      (MissingField ((StructType 121) z <opaque>)))
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 121))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 121))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 120) (st_sig_id 46)))))
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
             ((function_params ((t (StructType 121))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 121))))
                  (destructuring_let_rest true)))
                (Return (Reference (y2 IntegerType)))))))))))
        (T (Value (Type (StructType 121))))))
      (structs
       ((121
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields
           ((x (ResolvedReference (Integer <opaque>)))
            (y (ResolvedReference (Integer <opaque>)))
            (z (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods ()) (st_sig_base_id 120) (st_sig_id 46)))))
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
    (((TypeError ((InterfaceType 120) (Type0 (StructType 122)) <opaque>))
      (ArgumentNumberMismatch (1 1 <opaque>)))
     ((bindings
       ((Foo (Value (Type (StructType 122))))
        (ExpectedIntf
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 120))))
              (function_returns HoleType)))
            (function_impl (Fn (Block ())))))))
        (Intf (Value (Type (InterfaceType 120))))))
      (structs
       ((122
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 122) (uty_base_id 121)))))))
      (interfaces ((120 ((interface_methods ()))))) (type_counter <opaque>)
      (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 121)
          (st_sig_id 46)))))
      (union_signs (0 ())) (attr_executors <opaque>))) |}]

let%expect_test "struct signatures" =
  let source =
    {|
       struct Int2[bits: Integer] {
         val value: Integer
         fn new(i: Integer) -> Self {
           Self { value: i }
         }
       }
       fn extract_value[n: Integer](x: Int2[n]) -> Integer {
         x.value
       }
       let five = extract_value[10](Int2[10].new(5));
       let zero = extract_value[20](Int2[20].new(0));
     |}
  in
  pp_compile source ;
  [%expect
    {|
    (((FieldNotFoundF value) (UnresolvedIdentifier extract_value)
      (UnresolvedIdentifier extract_value))
     ((bindings
       ((Int2
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((bits IntegerType)))
              (function_returns (StructSig 46))))
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
                           (ExprType (Reference (Self (StructSig 46)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Value
                            (Struct
                             ((Reference (Self (StructSig 46)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (mk_impls ()) (mk_id 120) (mk_sig 46) (mk_span <opaque>))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
      (struct_signs
       (1
        (((st_sig_fields ((value (ResolvedReference (Integer <opaque>)))))
          (st_sig_methods
           ((new
             ((function_params ((i IntegerType)))
              (function_returns (ExprType (Reference (Self (StructSig 46)))))))))
          (st_sig_base_id 120) (st_sig_id 46)))))
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
       ((Ti (Value (Type (StructType 132)))) (U1 (Value (Type (UnionType 130))))
        (U (Value (Type (UnionType 127)))) (I (Value (Type (InterfaceType 125))))
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
        (T1 (Value (Type (StructType 124))))
        (Ta
         (Value
          (Function
           ((function_signature
             ((function_is_type) (function_params ((X IntegerType)))
              (function_returns (StructSig 47))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_attributes
                  (((attribute_ident attr) (attribute_exprs ()))))
                 (mk_struct_fields ())
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 122) (mk_sig 47)
                   (mk_span <opaque>))))))))))))
        (T (Value (Type (StructType 121))))))
      (structs
       ((132
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
               (impl_interface 125)
               (impl_methods
                ((x
                  ((function_signature
                    ((function_attributes
                      (((attribute_ident attr) (attribute_exprs ()))))
                     (function_params ()) (function_returns BoolType)))
                   (function_impl (Fn (Return (Value (Bool true))))))))))))
            (uty_id 132) (uty_base_id 131)))))
        (124
         ((struct_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 124) (uty_base_id 123)))))
        (121
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
            (uty_impls ()) (uty_id 121) (uty_base_id 120)))))))
      (unions
       ((130
         ((union_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (cases (((ExprType (Value Void)) (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 128)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v VoidType)))
                     (function_returns (UnionType 129))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v VoidType)) 130))))))))))))
            (uty_id 130) (uty_base_id 129)))))
        (127
         ((union_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (cases (((ExprType (Value Void)) (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 128)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v VoidType)))
                     (function_returns (UnionType 126))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v VoidType)) 127))))))))))))
            (uty_id 127) (uty_base_id 126)))))))
      (interfaces
       ((128
         ((interface_methods
           ((from
             ((function_params ((from VoidType))) (function_returns SelfType)))))))
        (125
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
          (st_sig_base_id 131) (st_sig_id 49))
         ((st_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 123)
          (st_sig_id 48))
         ((st_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (st_sig_fields ()) (st_sig_methods ()) (st_sig_base_id 122)
          (st_sig_id 47))
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
          (st_sig_base_id 120) (st_sig_id 46)))))
      (union_signs
       (2
        (((un_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (un_sig_cases ((ExprType (Value Void)))) (un_sig_methods ())
          (un_sig_base_id 129))
         ((un_sig_attributes (((attribute_ident attr) (attribute_exprs ()))))
          (un_sig_cases ((ExprType (Value Void)))) (un_sig_methods ())
          (un_sig_base_id 126)))))
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
                   ((assignment_ident a)
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
                        ((assignment_ident a)
                         (assignment_expr (Value (Integer 10))))))))
                    (if_else
                     ((Block
                       ((Assignment
                         ((assignment_ident a)
                          (assignment_expr (Value (Integer 20)))))))))))
                  (Return (Reference (a IntegerType)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)
        (struct_signs (0 ())) (union_signs (0 ())) (attr_executors <opaque>))) |}]

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
