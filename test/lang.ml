open Shared.Disabled
open Shared.Disabled.Lang

let add_bin_op_intf p =
  let bl = Tact.Located.Disabled.builtin_located in
  let intf =
    { interface_methods =
        [ ( "op",
            bl
              { function_params =
                  [(bl "left", IntegerType); (bl "right", IntegerType)];
                function_returns = IntegerType } ) ] }
  in
  let intf_ty =
    Value (Type (Lang.Program.insert_interface_with_id p (-10) intf))
  in
  {p with bindings = (bl "BinOp", bl intf_ty) :: p.bindings}

let%expect_test "program returns" =
  let sources =
    [{| 1 |}; {| return 1|}; {| fn x() { 1 } x() |}; {| struct T {} |}]
  in
  List.iter ~f:pp_compile sources ;
  [%expect
    {|
    (Ok
     ((bindings ()) (result (Integer 1)) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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
          (un_sig_base_id 20)))))))
    (Ok
                                    ((bindings ()) (result (Integer 1))
                                     (structs ()) (type_counter <opaque>)
                                     (memoized_fcalls <opaque>)
                                     (struct_signs (0 ()))
                                     (union_signs
                                      (5
                                       (((un_sig_cases
                                          ((StructType 59) (StructType 76)))
                                         (un_sig_methods ()) (un_sig_base_id 77))
                                        ((un_sig_cases ((StructType 55)))
                                         (un_sig_methods ()) (un_sig_base_id 60))
                                        ((un_sig_cases
                                          ((UnionType 21) (UnionType 39)))
                                         (un_sig_methods ()) (un_sig_base_id 44))
                                        ((un_sig_cases
                                          ((StructType 31) (StructType 35)))
                                         (un_sig_methods ()) (un_sig_base_id 38))
                                        ((un_sig_cases
                                          ((StructType 14) (StructType 18)))
                                         (un_sig_methods ()) (un_sig_base_id 20)))))))

    (Ok
     ((bindings
       ((x
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns IntegerType)))
            (function_impl (Fn (Return (Value (Integer 1)))))))))))
      (result (Integer 1)) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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
          (un_sig_base_id 20)))))))
    (Ok
                                    ((bindings
                                      ((T (Value (Type (StructType 84))))))
                                     (structs
                                      ((84
                                        ((struct_fields ())
                                         (struct_details
                                          ((uty_methods ()) (uty_impls ())
                                           (uty_id 84) (uty_base_id 83)))))))
                                     (type_counter <opaque>)
                                     (memoized_fcalls <opaque>)
                                     (struct_signs (0 ()))
                                     (union_signs
                                      (5
                                       (((un_sig_cases
                                          ((StructType 59) (StructType 76)))
                                         (un_sig_methods ()) (un_sig_base_id 77))
                                        ((un_sig_cases ((StructType 55)))
                                         (un_sig_methods ()) (un_sig_base_id 60))
                                        ((un_sig_cases
                                          ((UnionType 21) (UnionType 39)))
                                         (un_sig_methods ()) (un_sig_base_id 44))
                                        ((un_sig_cases
                                          ((StructType 31) (StructType 35)))
                                         (un_sig_methods ()) (un_sig_base_id 38))
                                        ((un_sig_cases
                                          ((StructType 14) (StructType 18)))
                                         (un_sig_methods ()) (un_sig_base_id 20))))))) |}]

let%expect_test "scope resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 83))))))
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

let%expect_test "binding resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 83))))))
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

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp_compile source ;
  [%expect
    {|
    (((UnresolvedIdentifier Int256))
     ((bindings ((T (Value Void)))) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((B (Value (Type (StructType 83)))) (A (Value (Type (StructType 83))))))
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

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 86))))))
      (structs
       ((86
         ((struct_fields ((t ((field_type (StructType 83))))))
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

let%expect_test "Tact function evaluation" =
  let source =
    {|
    fn test(i: Int(257)) -> Int(257) {
      i
    }
    let a = test(test(Int(257).new(1)));
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a
         (Value
          (Struct ((Value (Type (StructType 83))) ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 83))))
              (function_returns (StructType 83))))
            (function_impl (Fn (Return (Reference (i (StructType 83))))))))))))
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

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
       val a: Int(257)
       val b: Bool
  };
  |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((MyType (Value (Type (StructType 86))))))
      (structs
       ((86
         ((struct_fields
           ((a ((field_type (StructType 83)))) (b ((field_type BoolType)))))
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

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      val a: Int(257)
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
          ((a (Value (Type (StructType 83))))
           (a (ResolvedReference (Bool <opaque>)))))
         (mk_struct_details
          ((mk_methods ()) (mk_impls ()) (mk_id 85) (mk_sig 82)
           (mk_span <opaque>)))))))
     ((bindings ((MyType (Value (Type (StructType 86))))))
      (structs
       ((86
         ((struct_fields
           ((a ((field_type (StructType 83)))) (a ((field_type BoolType)))))
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

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T[A: Type] { val a: A }
      let TA = T(Int(257));
   |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((TA (Value (Type (StructType 86))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ((A (TypeN 0))))
              (function_returns (StructSig 82))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((a (Reference (A (TypeN 0))))))
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 83) (mk_sig 82)
                   (mk_span <opaque>))))))))))))))
      (structs
       ((86
         ((struct_fields ((a ((field_type (StructType 84))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 86) (uty_base_id 83)))))
        (85
         ((struct_fields
           ((slice ((field_type (StructType 6))))
            (value ((field_type (StructType 84))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((s (StructType 6)) (v (StructType 84))))
                  (function_returns (StructType 85))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 85)))
                      ((slice (Reference (s (StructType 6))))
                       (value (Reference (v (StructType 84)))))))))))))))
            (uty_impls ()) (uty_id 85) (uty_base_id -500)))))
        (84
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 84))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 84)))
                      ((value (Reference (i IntegerType))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 84)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 84))) value IntegerType))
                      (Value (Integer 257))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 6))))
                  (function_returns (StructType 85))))
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
                       ((Value (Type (StructType 85)))
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 84)))
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType)))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 84))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 84)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 84)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 84))) value IntegerType))
                         (Value (Integer 257)))))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 6))))
                     (function_returns (StructType 85))))
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
                          ((Value (Type (StructType 85)))
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 84)))
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType))))))))))))))))))))))
              ((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 84))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 84)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 84) (uty_base_id 9)))))))
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

let%expect_test "scoping that `let` introduces in code" =
  let source =
    {|
    fn f(i: Int(257)) {
      let a = i;
      a
    }
    let b = f(Int(257).new(1));
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b
         (Value
          (Struct ((Value (Type (StructType 83))) ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 83))))
              (function_returns (StructType 83))))
            (function_impl
             (Fn
              (Block
               ((Let ((a (Reference (i (StructType 83))))))
                (Return (Reference (a (StructType 83))))))))))))))
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

let%expect_test "reference in function bodies" =
  let source =
    {|
      fn op(i: Int(257), i_: Int(257)) {
        i
      }

      fn f(x: Int(257)) {
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
             ((function_params ((x (StructType 83))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((a
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (x (StructType 83)))
                      (Reference (x (StructType 83)))))))))
                (Let
                 ((b
                   (FunctionCall
                    ((ResolvedReference (op <opaque>))
                     ((Reference (a (StructType 83)))
                      (Reference (a (StructType 83)))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 83)) (i_ (StructType 83))))
              (function_returns (StructType 83))))
            (function_impl (Fn (Return (Reference (i (StructType 83))))))))))))
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
        (foo (Value (Struct ((Value (Type (StructType 84))) ()))))
        (Foo (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 84)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
     ((bindings ((res (Value (Integer 1))) (Foo (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
     ((bindings ((Foo (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (StructType 84))))
                  (function_returns (StructType 84))))
                (function_impl (Fn (Return (Reference (self (StructType 84))))))))))
            (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
       ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 84))))
        (make_foo
         (Value
          (Function
           ((function_signature
             ((function_params ((foo (UnionType 84))))
              (function_returns (UnionType 84))))
            (function_impl (Fn (Return (Reference (foo (UnionType 84))))))))))
        (Foo (Value (Type (UnionType 84))))))
      (structs ())
      (unions
       ((84
         ((cases ((BoolType (Discriminator 0))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((self (UnionType 84)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 85)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 84))))))))))))
            (uty_id 84) (uty_base_id 83)))))))
      (interfaces
       ((85
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases (BoolType)) (un_sig_methods ()) (un_sig_base_id 83))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

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
             ((function_params ((foo (UnionType 84))))
              (function_returns (UnionType 84))))
            (function_impl (Fn (Return (Reference (foo (UnionType 84))))))))))
        (Foo (Value (Type (UnionType 84))))))
      (structs ())
      (unions
       ((84
         ((cases ((BoolType (Discriminator 0))))
          (union_details
           ((uty_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (i IntegerType)))))))))
            (uty_impls
             (((impl_interface 85)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v BoolType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v BoolType)) 84))))))))))))
            (uty_id 84) (uty_base_id 83)))))))
      (interfaces
       ((85
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases (BoolType)) (un_sig_methods ()) (un_sig_base_id 83))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

let%expect_test "struct instantiation" =
  let source =
    {|
    struct T {
      val a: Integer
      val b: Integer
    }

    let t = T{a: 1, b: 2}
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
           ((Value (Type (StructType 84)))
            ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((StructType 50) (StructType 52))))
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 52))))
              (function_returns (StructType 50))))
            (function_impl (Fn (Return (Reference (i (StructType 52))))))))))))
      (structs ())
      (interfaces
       ((83
         ((interface_methods
           ((from
             ((function_params ((from (StructType 52))))
              (function_returns SelfType)))))))))
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
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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
          (un_sig_base_id 20)))))))
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
              (Break
               (If
                ((if_condition (Value (Bool true)))
                 (if_then (Block ((Return (Value (Integer 1))))))
                 (if_else ((Block ((Return (Value (Integer 2))))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns BoolType)))
            (function_impl (Fn (Return (Value (Bool true)))))))))))
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

let%expect_test "type check error" =
  let source =
    {|
      {
        fn foo(x: Int(99)) { return x; }

        let a = foo(Int(10).new(1))
      }
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((TypeError ((StructType 83) (StructType 85))))
     ((bindings ())
      (structs
       ((86
         ((struct_fields
           ((slice ((field_type (StructType 6))))
            (value ((field_type (StructType 85))))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((s (StructType 6)) (v (StructType 85))))
                  (function_returns (StructType 86))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 86)))
                      ((slice (Reference (s (StructType 6))))
                       (value (Reference (v (StructType 85)))))))))))))))
            (uty_impls ()) (uty_id 86) (uty_base_id -500)))))
        (85
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 85))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 85)))
                      ((value (Reference (i IntegerType))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 85)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  (Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 85))) value IntegerType))
                      (Value (Integer 10))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 6))))
                  (function_returns (StructType 86))))
                (function_impl
                 (Fn
                  (Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                    (Return
                     (Value
                      (Struct
                       ((Value (Type (StructType 86)))
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            ((Value (Type (StructType 85)))
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType)))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 85))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 85)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls
             (((impl_interface -1)
               (impl_methods
                ((serialize
                  ((function_signature
                    ((function_params
                      ((self (StructType 85)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     (Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 85))) value IntegerType))
                         (Value (Integer 10)))))))))))))
              ((impl_interface -2)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ((s (StructType 6))))
                     (function_returns (StructType 86))))
                   (function_impl
                    (Fn
                     (Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                       (Return
                        (Value
                         (Struct
                          ((Value (Type (StructType 86)))
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               ((Value (Type (StructType 85)))
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType))))))))))))))))))))))
              ((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 85))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 85)))
                         ((value (Reference (i IntegerType)))))))))))))))))
            (uty_id 85) (uty_base_id 9)))))
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
                      (Value (Integer 99))))))))))
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
                         ((Reference (s (StructType 6))) (Value (Integer 99))))))))
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
                         (Value (Integer 99)))))))))))))
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
                            ((Reference (s (StructType 6))) (Value (Integer 99))))))))
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
      (interfaces
       ((87
         ((interface_methods
           ((from
             ((function_params ((from (StructType 85))))
              (function_returns SelfType)))))))))
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
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 84))))))
      (structs
       ((84
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
       ((empty (Value (Struct ((Value (Type (StructType 85))) ()))))
        (Empty (Value (Type (StructType 85))))
        (Make (Value (Type (InterfaceType 83))))))
      (structs
       ((85
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ()) (function_returns (StructType 85))))
                (function_impl
                 (Fn
                  (Return (Value (Struct ((Value (Type (StructType 85))) ()))))))))))
            (uty_impls
             (((impl_interface 83)
               (impl_methods
                ((new
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 85))))
                   (function_impl
                    (Fn
                     (Return
                      (Value (Struct ((Value (Type (StructType 85))) ())))))))))))))
            (uty_id 85) (uty_base_id 84)))))))
      (interfaces
       ((83
         ((interface_methods
           ((new ((function_params ()) (function_returns SelfType)))))))))
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

let%expect_test "serializer inner struct" =
  let source =
    {|
      struct Inner { val x: Int(32) }
      struct Outer { val y: Int(32) val z: Inner }
      let serialize_outer = serializer(Outer);
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
                       ((Reference (self (StructType 86))) y (StructType 52)))
                      (Reference (b (StructType 3)))))))))
                (Return (Reference (b (StructType 3))))))))))))
        (Outer (Value (Type (StructType 86))))
        (Inner (Value (Type (StructType 84))))))
      (structs
       ((86
         ((struct_fields
           ((y ((field_type (StructType 52))))
            (z ((field_type (StructType 84))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 86) (uty_base_id 85)))))
        (84
         ((struct_fields ((x ((field_type (StructType 52))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
                 ((Reference (Y (TypeN 0)))))))))))))
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
    (((TypeError ((TypeN 0) (TypeN 1))))
     ((bindings
       ((must_fail (Value Void))
        (id
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
            (function_impl (Fn (Return (Reference (X (TypeN 0))))))))))))
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

let%expect_test "union variants constructing" =
  let source =
    {|
      union Uni {
        case Integer
        case Int(32)
      }
      fn test(value: Uni) -> Uni {
        value
      }
      let a = test(10);
      let b = test(Int(32).new(1));
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
             ((Value (Type (StructType 52))) ((value (Value (Integer 1))))))
            84))))
        (a (Value (UnionVariant ((Integer 10) 84))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 84))))
              (function_returns (UnionType 84))))
            (function_impl (Fn (Return (Reference (value (UnionType 84))))))))))
        (Uni (Value (Type (UnionType 84))))))
      (structs ())
      (unions
       ((84
         ((cases
           (((StructType 52) (Discriminator 1)) (IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v IntegerType)) 84))))))))))
              ((impl_interface 85)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 52))))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 52))) 84))))))))))))
            (uty_id 84) (uty_base_id 83)))))))
      (interfaces
       ((85
         ((interface_methods
           ((from
             ((function_params ((from (StructType 52))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases (IntegerType (StructType 52))) (un_sig_methods ())
          (un_sig_base_id 83))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

let%expect_test "unions duplicate variant" =
  let source =
    {|
      fn Test(T: Type) {
        union {
          case Integer
          case T
        }
      }
      let a = Test(builtin_Builder); // should be OK
      let b = Test(Integer); // should fail
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (((DuplicateVariant (IntegerType <opaque>)))
     ((bindings
       ((b (Value (Type (UnionType 86)))) (a (Value (Type (UnionType 84))))
        (Test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (TypeN 0)))) (function_returns (UnionSig 5))))
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
                    (((mk_impl_interface (Value (Type (InterfaceType 10))))
                      (mk_impl_methods
                       ((from
                         (Value
                          (Function
                           ((function_signature
                             ((function_params ((v IntegerType)))
                              (function_returns (UnionType 83))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference (v IntegerType)) 83))))))))))))
                     ((mk_impl_interface
                       (FunctionCall
                        ((Value
                          (Function
                           ((function_signature
                             ((function_params ((T (TypeN 0))))
                              (function_returns HoleType)))
                            (function_impl (BuiltinFn (<fun> <opaque>))))))
                         ((Value (Type (ExprType (Reference (T (TypeN 0))))))))))
                      (mk_impl_methods
                       ((from
                         (Value
                          (Function
                           ((function_signature
                             ((function_params
                               ((v (ExprType (Reference (T (TypeN 0)))))))
                              (function_returns (UnionType 83))))
                            (function_impl
                             (Fn
                              (Return
                               (MakeUnionVariant
                                ((Reference
                                  (v (ExprType (Reference (T (TypeN 0))))))
                                 83))))))))))))))
                   (mk_id 83) (mk_sig 5) (mk_span <opaque>))))))))))))))
      (structs ())
      (unions
       ((86
         ((cases ((IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v IntegerType)) 86))))))))))
              ((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v IntegerType)) 86))))))))))))
            (uty_id 86) (uty_base_id 83)))))
        (84
         ((cases
           (((BuiltinType Builder) (Discriminator 1))
            (IntegerType (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 10)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v IntegerType)))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return (MakeUnionVariant ((Reference (v IntegerType)) 84))))))))))
              ((impl_interface 85)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (BuiltinType Builder))))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant
                       ((Reference (v (BuiltinType Builder))) 84))))))))))))
            (uty_id 84) (uty_base_id 83)))))))
      (interfaces
       ((85
         ((interface_methods
           ((from
             ((function_params ((from (BuiltinType Builder))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases (IntegerType (ExprType (Reference (T (TypeN 0))))))
          (un_sig_methods ()) (un_sig_base_id 83))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

let%expect_test "unions" =
  let source =
    {|
      union Test {
        case Int(257)
        case Int(64)
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
     ((bindings ((Test (Value (Type (UnionType 86))))))
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
      (unions
       ((86
         ((cases
           (((StructType 50) (Discriminator 1))
            ((StructType 83) (Discriminator 0))))
          (union_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (UnionType 86))))
                  (function_returns (UnionType 86))))
                (function_impl (Fn (Return (Reference (self (UnionType 86))))))))))
            (uty_impls
             (((impl_interface 87)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 83))))
                     (function_returns (UnionType 85))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 83))) 86))))))))))
              ((impl_interface 88)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 50))))
                     (function_returns (UnionType 85))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 50))) 86))))))))))))
            (uty_id 86) (uty_base_id 85)))))))
      (interfaces
       ((88
         ((interface_methods
           ((from
             ((function_params ((from (StructType 50))))
              (function_returns SelfType)))))))
        (87
         ((interface_methods
           ((from
             ((function_params ((from (StructType 83))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases ((StructType 83) (StructType 50))) (un_sig_methods ())
          (un_sig_base_id 85))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

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
       ((y (Value (Struct ((Value (Type (StructType 86))) ()))))
        (foo_empty (Value (Struct ((Value (Type (StructType 87))) ()))))
        (Empty (Value (Type (StructType 86)))) (x (Value (Integer 10)))
        (foo (Value (Struct ((Value (Type (StructType 84))) ()))))
        (Foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0))))
              (function_returns (StructSig 82))))
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
                           ((self (ExprType (Reference (Self (StructSig 82)))))
                            (x (ExprType (Reference (X (TypeN 0)))))))
                          (function_returns (ExprType (Reference (X (TypeN 0)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))
                   (mk_impls ()) (mk_id 83) (mk_sig 82) (mk_span <opaque>))))))))))))))
      (structs
       ((87
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (StructType 87)) (x (StructType 86))))
                  (function_returns (StructType 86))))
                (function_impl (Fn (Return (Reference (x (StructType 86))))))))))
            (uty_impls ()) (uty_id 87) (uty_base_id 83)))))
        (86
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 86) (uty_base_id 85)))))
        (84
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((id
               ((function_signature
                 ((function_params ((self (StructType 84)) (x IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Reference (x IntegerType)))))))))
            (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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

let%expect_test "switch statement" =
  let source =
    {|
      union Ints {
        case Int(32)
        case Int(64)
      }
      fn test(i: Ints) -> Integer {
        switch (i) {
          case Int(32) vax => { return 32; }
          case Int(64) vax => { return 64; }
        }
      }
      let must_be_32 = test(Int(32).new(0));
      let must_be_64 = test(Int(64).new(0));
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
             ((function_params ((i (UnionType 84))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Break
               (Switch
                ((switch_condition (Reference (i (UnionType 84))))
                 (branches
                  (((branch_ty (StructType 52)) (branch_var vax)
                    (branch_stmt (Block ((Return (Value (Integer 32)))))))
                   ((branch_ty (StructType 50)) (branch_var vax)
                    (branch_stmt (Block ((Return (Value (Integer 64))))))))))))))))))
        (Ints (Value (Type (UnionType 84))))))
      (structs ())
      (unions
       ((84
         ((cases
           (((StructType 50) (Discriminator 1))
            ((StructType 52) (Discriminator 0))))
          (union_details
           ((uty_methods ())
            (uty_impls
             (((impl_interface 85)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 52))))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 52))) 84))))))))))
              ((impl_interface 86)
               (impl_methods
                ((from
                  ((function_signature
                    ((function_params ((v (StructType 50))))
                     (function_returns (UnionType 83))))
                   (function_impl
                    (Fn
                     (Return
                      (MakeUnionVariant ((Reference (v (StructType 50))) 84))))))))))))
            (uty_id 84) (uty_base_id 83)))))))
      (interfaces
       ((86
         ((interface_methods
           ((from
             ((function_params ((from (StructType 50))))
              (function_returns SelfType)))))))
        (85
         ((interface_methods
           ((from
             ((function_params ((from (StructType 52))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs
       (6
        (((un_sig_cases ((StructType 52) (StructType 50))) (un_sig_methods ())
          (un_sig_base_id 83))
         ((un_sig_cases ((StructType 59) (StructType 76))) (un_sig_methods ())
          (un_sig_base_id 77))
         ((un_sig_cases ((StructType 55))) (un_sig_methods ())
          (un_sig_base_id 60))
         ((un_sig_cases ((UnionType 21) (UnionType 39))) (un_sig_methods ())
          (un_sig_base_id 44))
         ((un_sig_cases ((StructType 31) (StructType 35))) (un_sig_methods ())
          (un_sig_base_id 38))
         ((un_sig_cases ((StructType 14) (StructType 18))) (un_sig_methods ())
          (un_sig_base_id 20))))))) |}]

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
                 ((Value (Integer 10)) (Reference (y IntegerType))))))))))))
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
                      ((Reference (x IntegerType)) (Reference (y IntegerType)))))))))))))))))
        (left
         (Value
          (Function
           ((function_signature
             ((function_params ((x IntegerType) (y IntegerType)))
              (function_returns IntegerType)))
            (function_impl (Fn (Return (Reference (x IntegerType)))))))))))
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

let%expect_test "let binding with type" =
  let source = {|
      let a: Int(257) = 1;
      let a: Int(32) = 2;
    |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a
         (Value
          (Struct ((Value (Type (StructType 52))) ((value (Value (Integer 2))))))))
        (a
         (Value
          (Struct ((Value (Type (StructType 83))) ((value (Value (Integer 1))))))))))
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

let%expect_test "let binding with a non-matching type" =
  let source = {|
      let a: Bool = 1;
    |} in
  pp_compile source ;
  [%expect
    {|
    (((TypeError (BoolType IntegerType)) (TypeError (BoolType IntegerType)))
     ((bindings ((a (Value Void)))) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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
             ((function_params ((t (StructType 85))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Return
               (FunctionCall
                ((Value
                  (Function
                   ((function_signature
                     ((function_params ((self (StructType 85))))
                      (function_returns IntegerType)))
                    (function_impl (Fn (Return (Value (Integer 1))))))))
                 ((Reference (t (StructType 85)))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 83))))
              (function_returns
               (FunctionType
                ((function_params
                  ((t (ExprType (Reference (T (InterfaceType 83)))))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params
                    ((t (ExprType (Reference (T (InterfaceType 83)))))))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Return
                    (IntfMethodCall
                     ((intf_instance (Reference (T (InterfaceType 83))))
                      (intf_def 83)
                      (intf_method
                       (beep
                        ((function_params ((self SelfType)))
                         (function_returns IntegerType))))
                      (intf_args
                       ((Reference
                         (t (ExprType (Reference (T (InterfaceType 83))))))))
                      (intf_loc <opaque>)))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 85))))
        (Beep (Value (Type (InterfaceType 83))))))
      (structs
       ((85
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((beep
               ((function_signature
                 ((function_params ((self (StructType 85))))
                  (function_returns IntegerType)))
                (function_impl (Fn (Return (Value (Integer 1)))))))))
            (uty_impls
             (((impl_interface 83)
               (impl_methods
                ((beep
                  ((function_signature
                    ((function_params ((self (StructType 85))))
                     (function_returns IntegerType)))
                   (function_impl (Fn (Return (Value (Integer 1))))))))))))
            (uty_id 85) (uty_base_id 84)))))))
      (interfaces
       ((83
         ((interface_methods
           ((beep
             ((function_params ((self SelfType))) (function_returns IntegerType)))))))))
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
             ((function_params ((t (StructType 84))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((x x) (y y2) (z z)))
                  (destructuring_let_expr (Reference (t (StructType 84))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 HoleType)))))))))))
        (T (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
    (((MissingField (84 x)) (MissingField (84 z)) (MissingField (84 x))
      (MissingField (84 z)))
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 84))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 84))))
                  (destructuring_let_rest false)))
                (Return (Reference (y2 HoleType)))))))))))
        (T (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
             ((function_params ((t (StructType 84))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              (Block
               ((DestructuringLet
                 ((destructuring_let ((y y2)))
                  (destructuring_let_expr (Reference (t (StructType 84))))
                  (destructuring_let_rest true)))
                (Return (Reference (y2 HoleType)))))))))))
        (T (Value (Type (StructType 84))))))
      (structs
       ((84
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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
    (((TypeError ((InterfaceType 83) (TypeN 0))))
     ((bindings
       ((a (Value Void)) (Foo (Value (Type (StructType 85))))
        (ExpectedIntf
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 83))))
              (function_returns HoleType)))
            (function_impl (Fn (Block ())))))))
        (Intf (Value (Type (InterfaceType 83))))))
      (structs
       ((85
         ((struct_fields ())
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 85) (uty_base_id 84)))))))
      (interfaces ((83 ((interface_methods ()))))) (type_counter <opaque>)
      (memoized_fcalls <opaque>) (struct_signs (0 ()))
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

let%expect_test "struct signatures" =
  let source =
    {|
       struct Int2[bits: Integer] {
         val value: Integer
         fn new(i: Integer) -> Self {
           Self { value: i }
         }
       }
       fn extract_value(n: Integer) {
         fn(x: Int2(n)) -> Integer {
           x.value
         }
       }
       let five = extract_value(10)(Int2(10).new(5));
       let zero = extract_value(20)(Int2(20).new(0));
     |}
  in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((zero (Value (Integer 0))) (five (Value (Integer 5)))
        (extract_value
         (Value
          (Function
           ((function_signature
             ((function_params ((n IntegerType)))
              (function_returns
               (FunctionType
                ((function_params
                  ((x
                    (ExprType
                     (FunctionCall
                      ((ResolvedReference (Int2 <opaque>))
                       ((Reference (n IntegerType)))))))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              (Return
               (MkFunction
                ((function_signature
                  ((function_params
                    ((x
                      (ExprType
                       (FunctionCall
                        ((ResolvedReference (Int2 <opaque>))
                         ((Reference (n IntegerType)))))))))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn
                   (Return
                    (StructField
                     ((Reference
                       (x
                        (ExprType
                         (FunctionCall
                          ((ResolvedReference (Int2 <opaque>))
                           ((Reference (n IntegerType))))))))
                      value IntegerType))))))))))))))
        (Int2
         (Value
          (Function
           ((function_signature
             ((function_params ((bits IntegerType)))
              (function_returns (StructSig 82))))
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
                           (ExprType (Reference (Self (StructSig 82)))))))
                        (function_impl
                         (Fn
                          (Return
                           (Value
                            (Struct
                             ((Reference (Self (StructSig 82)))
                              ((value (Reference (i IntegerType)))))))))))))))
                   (mk_impls ()) (mk_id 83) (mk_sig 82) (mk_span <opaque>))))))))))))))
      (structs
       ((85
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 85))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 85)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls ()) (uty_id 85) (uty_base_id 83)))))
        (84
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_details
           ((uty_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 84))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 84)))
                      ((value (Reference (i IntegerType))))))))))))))
            (uty_impls ()) (uty_id 84) (uty_base_id 83)))))))
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

let%expect_test "Deserilize intf with constraints" =
  let source =
    {|
      struct Container(X: Type) { val x: X }
      interface Deserialize2 {
        fn deserialize() -> Container(Self)
      }
      fn test(Y: Deserialize2) -> Y {
        let v = Y.deserialize();
        v.x
      }

      struct Empty { 
        impl Deserialize2 { 
          fn deserialize() -> Container(Self) { 
            Container(Self) { x: Self {} }
          } 
        }
      }
      let a = test(Empty);
    |}
  in
  pp_compile source ~include_std:false ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Struct ((Value (Type (StructType 3))) ()))))
        (Empty (Value (Type (StructType 3))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((Y (InterfaceType 1))))
              (function_returns (ExprType (Reference (Y (InterfaceType 1)))))))
            (function_impl
             (Fn
              (Block
               ((Let
                 ((v
                   (IntfMethodCall
                    ((intf_instance (Reference (Y (InterfaceType 1))))
                     (intf_def 1)
                     (intf_method
                      (deserialize
                       ((function_params ())
                        (function_returns
                         (ExprType
                          (FunctionCall
                           ((Value
                             (Function
                              ((function_signature
                                ((function_params ((X (TypeN 0))))
                                 (function_returns (StructSig 1))))
                               (function_impl
                                (Fn
                                 (Return
                                  (MkStructDef
                                   ((mk_struct_fields
                                     ((x (Reference (X (TypeN 0))))))
                                    (mk_struct_details
                                     ((mk_methods ()) (mk_impls ()) (mk_id 0)
                                      (mk_sig 1) (mk_span <opaque>)))))))))))
                            ((ResolvedReference (Self <opaque>))))))))))
                     (intf_args ()) (intf_loc <opaque>))))))
                (Return
                 (StructField
                  ((Reference
                    (v
                     (ExprType
                      (FunctionCall
                       ((Value
                         (Function
                          ((function_signature
                            ((function_params ((X (TypeN 0))))
                             (function_returns (StructSig 1))))
                           (function_impl
                            (Fn
                             (Return
                              (MkStructDef
                               ((mk_struct_fields
                                 ((x (Reference (X (TypeN 0))))))
                                (mk_struct_details
                                 ((mk_methods ()) (mk_impls ()) (mk_id 0)
                                  (mk_sig 1) (mk_span <opaque>)))))))))))
                        ((ResolvedReference (Self <opaque>))))))))
                   x (ExprType (Reference (Y (InterfaceType 1)))))))))))))))
        (Deserialize2 (Value (Type (InterfaceType 1))))
        (Container
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0)))) (function_returns (StructSig 1))))
            (function_impl
             (Fn
              (Return
               (MkStructDef
                ((mk_struct_fields ((x (Reference (X (TypeN 0))))))
                 (mk_struct_details
                  ((mk_methods ()) (mk_impls ()) (mk_id 0) (mk_sig 1)
                   (mk_span <opaque>))))))))))))))
      (structs
       ((4
         ((struct_fields ((x ((field_type (StructType 3))))))
          (struct_details
           ((uty_methods ()) (uty_impls ()) (uty_id 4) (uty_base_id 0)))))
        (3
         ((struct_fields ())
          (struct_details
           ((uty_methods
             ((deserialize
               ((function_signature
                 ((function_params ()) (function_returns (StructType 4))))
                (function_impl
                 (Fn
                  (Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 4)))
                      ((x (Value (Struct ((Value (Type (StructType 3))) ())))))))))))))))
            (uty_impls
             (((impl_interface 1)
               (impl_methods
                ((deserialize
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 4))))
                   (function_impl
                    (Fn
                     (Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 4)))
                         ((x (Value (Struct ((Value (Type (StructType 3))) ()))))))))))))))))))
            (uty_id 3) (uty_base_id 2)))))))
      (interfaces
       ((1
         ((interface_methods
           ((deserialize
             ((function_params ())
              (function_returns
               (ExprType
                (FunctionCall
                 ((Value
                   (Function
                    ((function_signature
                      ((function_params ((X (TypeN 0))))
                       (function_returns (StructSig 1))))
                     (function_impl
                      (Fn
                       (Return
                        (MkStructDef
                         ((mk_struct_fields ((x (Reference (X (TypeN 0))))))
                          (mk_struct_details
                           ((mk_methods ()) (mk_impls ()) (mk_id 0) (mk_sig 1)
                            (mk_span <opaque>)))))))))))
                  ((ResolvedReference (Self <opaque>)))))))))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>) (struct_signs (0 ()))
      (union_signs (0 ())))) |}]
