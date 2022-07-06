open Shared
open Tact.Lang_types

let incr_f =
  Function
    { function_signature =
        { function_params = [("value", IntegerType)];
          function_returns = IntegerType };
      function_impl =
        BuiltinFn
          (builtin_fun (fun _p -> function
             | Integer arg :: _ ->
                 Integer (Zint.succ arg)
             | _ ->
                 Integer Zint.zero ) ) }

let add_bin_op_intf p =
  let intf =
    { interface_methods =
        [ ( "op",
            { function_params = [("left", IntegerType); ("right", IntegerType)];
              function_returns = IntegerType } ) ] }
  in
  let intf_ty =
    Value (Type (Lang.Program.insert_interface_with_id p (-10) intf))
  in
  {p with bindings = ("BinOp", intf_ty) :: p.bindings}

let%expect_test "scope resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((T (Value (Type (StructType 66))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "binding resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((T (Value (Type (StructType 66))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (UnresolvedIdentifier Int256))
      ((bindings ()) (structs ()) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((B (Value (Type (StructType 66)))) (A (Value (Type (StructType 66))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((T (Value (Type (StructType 68))))))
       (structs
        ((68
          ((struct_fields ((t ((field_type (StructType 66))))))
           (struct_methods ()) (struct_impls ()) (struct_id 68)))
         (66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp_compile source
    ~prev_program:
      { (Lang.default_program ()) with
        bindings = ("incr", Value incr_f) :: Lang.default_bindings () } ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((v (Value (Integer 4))))) (structs ()) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((a
          (Value
           (Struct
            ((Value (Type (StructType 66))) ((value (Value (Integer 1))))))))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ((i (StructType 66))))
               (function_returns (StructType 66))))
             (function_impl (Fn ((Return (Reference (i (StructType 66)))))))))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "compile-time function evaluation within a function" =
  let source =
    {|
    fn test() {
      let v = incr(incr(incr(1)));
      v
    }
  |}
  in
  pp_compile source
    ~prev_program:
      { (Lang.default_program ()) with
        bindings = ("incr", Value incr_f) :: Lang.default_bindings () } ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Block
                 ((Let ((v (Value (Integer 4)))))
                  (Return (ResolvedReference (v <opaque>))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((MyType (Value (Type (StructType 68))))))
       (structs
        ((68
          ((struct_fields
            ((a ((field_type (StructType 66)))) (b ((field_type BoolType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 68)))
         (66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType))
       (DuplicateField
        (a
         ((mk_struct_fields
           ((a (Value (Type (StructType 66)))) (a (Value (Type BoolType)))))
          (mk_methods ()) (mk_impls ()) (mk_struct_id -1)))))
      ((bindings ((MyType (Value (Type (StructType 68))))))
       (structs
        ((68
          ((struct_fields
            ((a ((field_type (StructType 66)))) (a ((field_type BoolType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 68)))
         (66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T(A: Type) { val a: A }
      let TA = T(Int(257));
   |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((TA (Value (Type (StructType 68))))
         (T
          (Value
           (Function
            ((function_signature
              ((function_params ((A (TypeN 0))))
               (function_returns
                (StructSig
                 ((st_sig_fields ((a (Value (Type (Dependent A (TypeN 0))))))))))))
             (function_impl
              (Fn
               ((Return
                 (MkStructDef
                  ((mk_struct_fields ((a (Reference (A (TypeN 0))))))
                   (mk_methods ()) (mk_impls ()) (mk_struct_id 66)))))))))))))
       (structs
        ((68
          ((struct_fields ((a ((field_type (StructType 67))))))
           (struct_methods ()) (struct_impls ()) (struct_id 68)))
         (67
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 67))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 67)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 67)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 67))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 67))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 67)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 67)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 67))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 67)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((a (Value (Integer 1)))
         (f
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns IntegerType)))
             (function_impl (Fn ((Return (Value (Integer 1))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((b
          (Value
           (Struct
            ((Value (Type (StructType 66))) ((value (Value (Integer 1))))))))
         (f
          (Value
           (Function
            ((function_signature
              ((function_params ((i (StructType 66))))
               (function_returns (StructType 66))))
             (function_impl
              (Fn
               ((Block
                 ((Let ((a (Reference (i (StructType 66))))))
                  (Return (Reference (a (StructType 66)))))))))))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((f
          (Value
           (Function
            ((function_signature
              ((function_params ((x (StructType 66))))
               (function_returns HoleType)))
             (function_impl
              (Fn
               ((Block
                 ((Let
                   ((a
                     (FunctionCall
                      ((ResolvedReference (op <opaque>))
                       ((Reference (x (StructType 66)))
                        (Reference (x (StructType 66)))))))))
                  (Let
                   ((b
                     (FunctionCall
                      ((ResolvedReference (op <opaque>))
                       ((Reference (a (StructType 66)))
                        (Reference (a (StructType 66))))))))))))))))))
         (op
          (Value
           (Function
            ((function_signature
              ((function_params ((i (StructType 66)) (i_ (StructType 66))))
               (function_returns (StructType 66))))
             (function_impl (Fn ((Return (Reference (i (StructType 66)))))))))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((x (Value (Integer 1)))
         (f
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns IntegerType)))
             (function_impl (Fn ((Return (ResolvedReference (i <opaque>))))))))))
         (i (Value (Integer 1)))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((res (Value (Integer 1)))
         (foo (Value (Struct ((Value (Type (StructType 67))) ()))))
         (Foo (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields ())
           (struct_methods
            ((bar
              ((function_signature
                ((function_params ((self (StructType 67)) (i IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
           (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((res (Value (Integer 1))) (Foo (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields ())
           (struct_methods
            ((bar
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
           (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((Foo (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields ())
           (struct_methods
            ((bar
              ((function_signature
                ((function_params ((self (StructType 67))))
                 (function_returns (StructType 67))))
               (function_impl (Fn ((Return (Reference (self (StructType 67)))))))))))
           (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 67))))
         (make_foo
          (Value
           (Function
            ((function_signature
              ((function_params ((foo (UnionType 67))))
               (function_returns (UnionType 67))))
             (function_impl (Fn ((Return (Reference (foo (UnionType 67)))))))))))
         (Foo (Value (Type (UnionType 67))))))
       (structs ())
       (unions
        ((67
          ((cases ((BoolType (Discriminator 0))))
           (union_methods
            ((bar
              ((function_signature
                ((function_params ((self (UnionType 67)) (i IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
           (union_impls
            (((impl_interface 68)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v BoolType)))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v BoolType)) 67)))))))))))))
           (union_id 67)))))
       (interfaces
        ((68
          ((interface_methods
            ((from
              ((function_params ((from BoolType))) (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((res (Value (Integer 1)))
         (make_foo
          (Value
           (Function
            ((function_signature
              ((function_params ((foo (UnionType 67))))
               (function_returns (UnionType 67))))
             (function_impl (Fn ((Return (Reference (foo (UnionType 67)))))))))))
         (Foo (Value (Type (UnionType 67))))))
       (structs ())
       (unions
        ((67
          ((cases ((BoolType (Discriminator 0))))
           (union_methods
            ((bar
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
           (union_impls
            (((impl_interface 68)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v BoolType)))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v BoolType)) 67)))))))))))))
           (union_id 67)))))
       (interfaces
        ((68
          ((interface_methods
            ((from
              ((function_params ((from BoolType))) (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((t
          (Value
           (Struct
            ((Value (Type (StructType 67)))
             ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
         (T (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields
            ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp_compile source ;
  [%expect
    {|
    (StructType 39)(StructType 40)(Error
                                   (((UnexpectedType (TypeN 0))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (UnexpectedType VoidType)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (UnexpectedType (TypeN 0))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (UnexpectedType VoidType)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (TypeError
                                      ((StructType 39) (StructType 40))))
                                    ((bindings
                                      ((foo
                                        (Value
                                         (Function
                                          ((function_signature
                                            ((function_params
                                              ((i (StructType 40))))
                                             (function_returns (StructType 39))))
                                           (function_impl
                                            (Fn
                                             ((Return
                                               (Reference (i (StructType 40)))))))))))))
                                     (structs ())
                                     (interfaces
                                      ((66
                                        ((interface_methods
                                          ((from
                                            ((function_params
                                              ((from (StructType 40))))
                                             (function_returns SelfType)))))))))
                                     (type_counter <opaque>)
                                     (memoized_fcalls <opaque>)))) |}]

let%expect_test "type inference" =
  let source = {|
      fn foo(i: Integer) { return i; }
    |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((foo
          (Value
           (Function
            ((function_signature
              ((function_params ((i IntegerType)))
               (function_returns IntegerType)))
             (function_impl (Fn ((Return (Reference (i IntegerType))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "scope doesn't leak bindings" =
  let source = {|
    {
     let a = 1;
    }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ()) (structs ()) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((a (Value (Integer 1)))
         (T
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Break
                 (If
                  ((if_condition (Value (Bool true)))
                   (if_then (Block ((Return (Value (Integer 1))))))
                   (if_else ((Block ((Return (Value (Integer 2)))))))))))))))))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ()) (function_returns BoolType)))
             (function_impl (Fn ((Return (Value (Bool true))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (StructType 66)(StructType 67)(Error
                                   (((UnexpectedType (TypeN 0))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (UnexpectedType VoidType)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (UnexpectedType (TypeN 0))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (UnexpectedType VoidType)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (FieldNotFoundF slice)
                                     (FieldNotFoundF value)
                                     (MethodNotFound
                                      ((Value (Type (TypeN 0))) new))
                                     (TypeError ((TypeN 0) VoidType))
                                     (TypeError
                                      ((StructType 66) (StructType 67))))
                                    ((bindings ())
                                     (structs
                                      ((67
                                        ((struct_fields
                                          ((value ((field_type IntegerType)))))
                                         (struct_methods
                                          ((new
                                            ((function_signature
                                              ((function_params
                                                ((i IntegerType)))
                                               (function_returns (StructType 67))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (Value
                                                  (Struct
                                                   ((Value
                                                     (Type (StructType 67)))
                                                    ((value
                                                      (Reference (i IntegerType)))))))))))))
                                           (serialize
                                            ((function_signature
                                              ((function_params
                                                ((self (StructType 67))
                                                 (builder (StructType 3))))
                                               (function_returns (StructType 3))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (FunctionCall
                                                  ((ResolvedReference
                                                    (serialize_int <opaque>))
                                                   ((Reference
                                                     (builder (StructType 3)))
                                                    (StructField
                                                     ((Reference
                                                       (self (StructType 67)))
                                                      value IntegerType))
                                                    (Value (Integer 10)))))))))))
                                           (deserialize
                                            ((function_signature
                                              ((function_params
                                                ((s (StructType 6))))
                                               (function_returns (TypeN 0))))
                                             (function_impl
                                              (Fn
                                               ((Block
                                                 ((Let
                                                   ((res
                                                     (FunctionCall
                                                      ((ResolvedReference
                                                        (load_int <opaque>))
                                                       ((Reference
                                                         (s (StructType 6)))
                                                        (Value (Integer 10))))))))
                                                  (Return
                                                   (Value
                                                    (Struct
                                                     ((Value (Type VoidType)) ())))))))))))
                                           (from
                                            ((function_signature
                                              ((function_params
                                                ((i IntegerType)))
                                               (function_returns (StructType 67))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (Value
                                                  (Struct
                                                   ((Value
                                                     (Type (StructType 67)))
                                                    ((value
                                                      (Reference (i IntegerType)))))))))))))))
                                         (struct_impls
                                          (((impl_interface -1)
                                            (impl_methods
                                             ((serialize
                                               ((function_signature
                                                 ((function_params
                                                   ((self (StructType 67))
                                                    (builder (StructType 3))))
                                                  (function_returns
                                                   (StructType 3))))
                                                (function_impl
                                                 (Fn
                                                  ((Return
                                                    (FunctionCall
                                                     ((ResolvedReference
                                                       (serialize_int <opaque>))
                                                      ((Reference
                                                        (builder (StructType 3)))
                                                       (StructField
                                                        ((Reference
                                                          (self (StructType 67)))
                                                         value IntegerType))
                                                       (Value (Integer 10))))))))))))))
                                           ((impl_interface -3)
                                            (impl_methods
                                             ((deserialize
                                               ((function_signature
                                                 ((function_params
                                                   ((s (StructType 6))))
                                                  (function_returns (TypeN 0))))
                                                (function_impl
                                                 (Fn
                                                  ((Block
                                                    ((Let
                                                      ((res
                                                        (FunctionCall
                                                         ((ResolvedReference
                                                           (load_int <opaque>))
                                                          ((Reference
                                                            (s (StructType 6)))
                                                           (Value (Integer 10))))))))
                                                     (Return
                                                      (Value
                                                       (Struct
                                                        ((Value (Type VoidType))
                                                         ()))))))))))))))
                                           ((impl_interface 10)
                                            (impl_methods
                                             ((from
                                               ((function_signature
                                                 ((function_params
                                                   ((i IntegerType)))
                                                  (function_returns
                                                   (StructType 67))))
                                                (function_impl
                                                 (Fn
                                                  ((Return
                                                    (Value
                                                     (Struct
                                                      ((Value
                                                        (Type (StructType 67)))
                                                       ((value
                                                         (Reference
                                                          (i IntegerType))))))))))))))))))
                                         (struct_id 67)))
                                       (66
                                        ((struct_fields
                                          ((value ((field_type IntegerType)))))
                                         (struct_methods
                                          ((new
                                            ((function_signature
                                              ((function_params
                                                ((i IntegerType)))
                                               (function_returns (StructType 66))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (Value
                                                  (Struct
                                                   ((Value
                                                     (Type (StructType 66)))
                                                    ((value
                                                      (Reference (i IntegerType)))))))))))))
                                           (serialize
                                            ((function_signature
                                              ((function_params
                                                ((self (StructType 66))
                                                 (builder (StructType 3))))
                                               (function_returns (StructType 3))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (FunctionCall
                                                  ((ResolvedReference
                                                    (serialize_int <opaque>))
                                                   ((Reference
                                                     (builder (StructType 3)))
                                                    (StructField
                                                     ((Reference
                                                       (self (StructType 66)))
                                                      value IntegerType))
                                                    (Value (Integer 99)))))))))))
                                           (deserialize
                                            ((function_signature
                                              ((function_params
                                                ((s (StructType 6))))
                                               (function_returns (TypeN 0))))
                                             (function_impl
                                              (Fn
                                               ((Block
                                                 ((Let
                                                   ((res
                                                     (FunctionCall
                                                      ((ResolvedReference
                                                        (load_int <opaque>))
                                                       ((Reference
                                                         (s (StructType 6)))
                                                        (Value (Integer 99))))))))
                                                  (Return
                                                   (Value
                                                    (Struct
                                                     ((Value (Type VoidType)) ())))))))))))
                                           (from
                                            ((function_signature
                                              ((function_params
                                                ((i IntegerType)))
                                               (function_returns (StructType 66))))
                                             (function_impl
                                              (Fn
                                               ((Return
                                                 (Value
                                                  (Struct
                                                   ((Value
                                                     (Type (StructType 66)))
                                                    ((value
                                                      (Reference (i IntegerType)))))))))))))))
                                         (struct_impls
                                          (((impl_interface -1)
                                            (impl_methods
                                             ((serialize
                                               ((function_signature
                                                 ((function_params
                                                   ((self (StructType 66))
                                                    (builder (StructType 3))))
                                                  (function_returns
                                                   (StructType 3))))
                                                (function_impl
                                                 (Fn
                                                  ((Return
                                                    (FunctionCall
                                                     ((ResolvedReference
                                                       (serialize_int <opaque>))
                                                      ((Reference
                                                        (builder (StructType 3)))
                                                       (StructField
                                                        ((Reference
                                                          (self (StructType 66)))
                                                         value IntegerType))
                                                       (Value (Integer 99))))))))))))))
                                           ((impl_interface -3)
                                            (impl_methods
                                             ((deserialize
                                               ((function_signature
                                                 ((function_params
                                                   ((s (StructType 6))))
                                                  (function_returns (TypeN 0))))
                                                (function_impl
                                                 (Fn
                                                  ((Block
                                                    ((Let
                                                      ((res
                                                        (FunctionCall
                                                         ((ResolvedReference
                                                           (load_int <opaque>))
                                                          ((Reference
                                                            (s (StructType 6)))
                                                           (Value (Integer 99))))))))
                                                     (Return
                                                      (Value
                                                       (Struct
                                                        ((Value (Type VoidType))
                                                         ()))))))))))))))
                                           ((impl_interface 10)
                                            (impl_methods
                                             ((from
                                               ((function_signature
                                                 ((function_params
                                                   ((i IntegerType)))
                                                  (function_returns
                                                   (StructType 66))))
                                                (function_impl
                                                 (Fn
                                                  ((Return
                                                    (Value
                                                     (Struct
                                                      ((Value
                                                        (Type (StructType 66)))
                                                       ((value
                                                         (Reference
                                                          (i IntegerType))))))))))))))))))
                                         (struct_id 66)))))
                                     (interfaces
                                      ((68
                                        ((interface_methods
                                          ((from
                                            ((function_params
                                              ((from (StructType 67))))
                                             (function_returns SelfType)))))))))
                                     (type_counter <opaque>)
                                     (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((one (Value (Integer 1))) (Left (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields ())
           (struct_methods
            ((op
              ((function_signature
                ((function_params ((left IntegerType) (right IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (left IntegerType))))))))))
           (struct_impls
            (((impl_interface -10)
              (impl_methods
               ((op
                 ((function_signature
                   ((function_params ((left IntegerType) (right IntegerType)))
                    (function_returns IntegerType)))
                  (function_impl (Fn ((Return (Reference (left IntegerType)))))))))))))
           (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((empty (Value (Struct ((Value (Type (StructType 68))) ()))))
         (Empty (Value (Type (StructType 68))))
         (Make (Value (Type (InterfaceType 66))))))
       (structs
        ((68
          ((struct_fields ())
           (struct_methods
            ((new
              ((function_signature
                ((function_params ()) (function_returns (StructType 68))))
               (function_impl
                (Fn
                 ((Return (Value (Struct ((Value (Type (StructType 68))) ())))))))))))
           (struct_impls
            (((impl_interface 66)
              (impl_methods
               ((new
                 ((function_signature
                   ((function_params ()) (function_returns (StructType 68))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value (Struct ((Value (Type (StructType 68))) ()))))))))))))))
           (struct_id 68)))))
       (interfaces
        ((66
          ((interface_methods
            ((new ((function_params ()) (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((serialize_outer
          (Value
           (Function
            ((function_signature
              ((function_params ((self (StructType 69)) (b (StructType 3))))
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
                             ((self (StructType 40)) (builder (StructType 3))))
                            (function_returns (StructType 3))))
                          (function_impl
                           (Fn
                            ((Return
                              (FunctionCall
                               ((ResolvedReference (serialize_int <opaque>))
                                ((Reference (builder (StructType 3)))
                                 (StructField
                                  ((Reference (self (StructType 40))) value
                                   IntegerType))
                                 (Value (Integer 32))))))))))))
                       ((StructField
                         ((Reference (self (StructType 69))) y (StructType 40)))
                        (Reference (b (StructType 3)))))))))
                  (Return (Reference (b (StructType 3)))))))))))))
         (Outer (Value (Type (StructType 69))))
         (Inner (Value (Type (StructType 67))))))
       (structs
        ((69
          ((struct_fields
            ((y ((field_type (StructType 40))))
             (z ((field_type (StructType 67))))))
           (struct_methods ()) (struct_impls ()) (struct_id 69)))
         (67
          ((struct_fields ((x ((field_type (StructType 40))))))
           (struct_methods ()) (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((one (Value (Integer 1)))
         (foo
          (Value
           (Function
            ((function_signature
              ((function_params ((X (TypeN 0))))
               (function_returns
                (FunctionType
                 ((function_params ((x (Dependent X (TypeN 0)))))
                  (function_returns (Dependent X (TypeN 0))))))))
             (function_impl
              (Fn
               ((Return
                 (MkFunction
                  ((function_signature
                    ((function_params ((x (ExprType (Reference (X (TypeN 0)))))))
                     (function_returns (ExprType (Reference (X (TypeN 0)))))))
                   (function_impl
                    (Fn
                     ((Return
                       (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((Y (TypeN 0))))
               (function_returns
                (FunctionType
                 ((function_params ((x (Dependent Y (TypeN 0)))))
                  (function_returns (Dependent Y (TypeN 0))))))))
             (function_impl
              (Fn
               ((Return
                 (FunctionCall
                  ((ResolvedReference (identity <opaque>))
                   ((Reference (Y (TypeN 0))))))))))))))
         (identity
          (Value
           (Function
            ((function_signature
              ((function_params ((X (TypeN 0))))
               (function_returns
                (FunctionType
                 ((function_params ((x (Dependent X (TypeN 0)))))
                  (function_returns (Dependent X (TypeN 0))))))))
             (function_impl
              (Fn
               ((Block
                 ((Let
                   ((f
                     (MkFunction
                      ((function_signature
                        ((function_params
                          ((x (ExprType (Reference (X (TypeN 0)))))))
                         (function_returns (ExprType (Reference (X (TypeN 0)))))))
                       (function_impl
                        (Fn
                         ((Return
                           (Reference (x (ExprType (Reference (X (TypeN 0)))))))))))))))
                  (Return
                   (Reference
                    (f
                     (FunctionType
                      ((function_params
                        ((x (ExprType (Reference (X (TypeN 0)))))))
                       (function_returns (ExprType (Reference (X (TypeN 0)))))))))))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (TypeError ((TypeN 0) (TypeN 1))))
      ((bindings
        ((must_fail (Value Void))
         (id
          (Value
           (Function
            ((function_signature
              ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
             (function_impl (Fn ((Return (Reference (X (TypeN 0)))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((b
          (Value
           (UnionVariant
            ((Struct
              ((Value (Type (StructType 40))) ((value (Value (Integer 1))))))
             67))))
         (a (Value (UnionVariant ((Integer 10) 67))))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ((value (UnionType 67))))
               (function_returns (UnionType 67))))
             (function_impl (Fn ((Return (Reference (value (UnionType 67)))))))))))
         (Uni (Value (Type (UnionType 67))))))
       (structs ())
       (unions
        ((67
          ((cases
            (((StructType 40) (Discriminator 1)) (IntegerType (Discriminator 0))))
           (union_methods ())
           (union_impls
            (((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v IntegerType)))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v IntegerType)) 67)))))))))))
             ((impl_interface 68)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (StructType 40))))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant ((Reference (v (StructType 40))) 67)))))))))))))
           (union_id 67)))))
       (interfaces
        ((68
          ((interface_methods
            ((from
              ((function_params ((from (StructType 40))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (DuplicateVariant IntegerType))
      ((bindings
        ((b (Value (Type (UnionType 69)))) (a (Value (Type (UnionType 67))))
         (Test
          (Value
           (Function
            ((function_signature
              ((function_params ((T (TypeN 0)))) (function_returns (TypeN 0))))
             (function_impl
              (Fn
               ((Return
                 (MkUnionDef
                  ((mk_cases
                    ((ResolvedReference (Integer <opaque>))
                     (Reference (T (TypeN 0)))))
                   (mk_union_methods ())
                   (mk_union_impls
                    (((mk_impl_interface
                       (FunctionCall
                        ((Value
                          (Function
                           ((function_signature
                             ((function_params ((T (TypeN 0))))
                              (function_returns HoleType)))
                            (function_impl (BuiltinFn (<fun> <opaque>))))))
                         ((Value (Type IntegerType))))))
                      (mk_impl_methods
                       ((from
                         (Value
                          (Function
                           ((function_signature
                             ((function_params ((v IntegerType)))
                              (function_returns (UnionType 66))))
                            (function_impl
                             (Fn
                              ((Return
                                (MakeUnionVariant
                                 ((Reference (v IntegerType)) 66)))))))))))))
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
                              (function_returns (UnionType 66))))
                            (function_impl
                             (Fn
                              ((Return
                                (MakeUnionVariant
                                 ((Reference
                                   (v (ExprType (Reference (T (TypeN 0))))))
                                  66)))))))))))))))
                   (mk_union_id 66)))))))))))))
       (structs ())
       (unions
        ((69
          ((cases ((IntegerType (Discriminator 0)))) (union_methods ())
           (union_impls
            (((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v IntegerType)))
                    (function_returns (UnionType 69))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v IntegerType)) 69)))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (ExprType (Reference (T (TypeN 0)))))))
                    (function_returns (UnionType 69))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant
                       ((Reference (v (ExprType (Reference (T (TypeN 0)))))) 69)))))))))))))
           (union_id 69)))
         (67
          ((cases
            (((BuiltinType Builder) (Discriminator 1))
             (IntegerType (Discriminator 0))))
           (union_methods ())
           (union_impls
            (((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v IntegerType)))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v IntegerType)) 67)))))))))))
             ((impl_interface 68)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (ExprType (Reference (T (TypeN 0)))))))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant
                       ((Reference (v (ExprType (Reference (T (TypeN 0)))))) 67)))))))))))))
           (union_id 67)))))
       (interfaces
        ((68
          ((interface_methods
            ((from
              ((function_params ((from (BuiltinType Builder))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings ((Test (Value (Type (UnionType 68))))))
       (structs
        ((66
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 66)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 66))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 257))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 66))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 66)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 66)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 66))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6)))
                             (Value (Integer 257))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 66))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 66)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 66)))))
       (unions
        ((68
          ((cases
            (((StructType 39) (Discriminator 1))
             ((StructType 66) (Discriminator 0))))
           (union_methods
            ((id
              ((function_signature
                ((function_params ((self (UnionType 68))))
                 (function_returns (UnionType 68))))
               (function_impl (Fn ((Return (Reference (self (UnionType 68)))))))))))
           (union_impls
            (((impl_interface 69)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (StructType 66))))
                    (function_returns (UnionType 68))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant ((Reference (v (StructType 66))) 68)))))))))))
             ((impl_interface 70)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (StructType 39))))
                    (function_returns (UnionType 68))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant ((Reference (v (StructType 39))) 68)))))))))))))
           (union_id 68)))))
       (interfaces
        ((70
          ((interface_methods
            ((from
              ((function_params ((from (StructType 39))))
               (function_returns SelfType)))))))
         (69
          ((interface_methods
            ((from
              ((function_params ((from (StructType 66))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((y (Value (Struct ((Value (Type (StructType 69))) ()))))
         (foo_empty (Value (Struct ((Value (Type (StructType 70))) ()))))
         (Empty (Value (Type (StructType 69)))) (x (Value (Integer 10)))
         (foo (Value (Struct ((Value (Type (StructType 67))) ()))))
         (Foo
          (Value
           (Function
            ((function_signature
              ((function_params ((X (TypeN 0))))
               (function_returns (StructSig ((st_sig_fields ()))))))
             (function_impl
              (Fn
               ((Return
                 (MkStructDef
                  ((mk_struct_fields ())
                   (mk_methods
                    ((id
                      (MkFunction
                       ((function_signature
                         ((function_params
                           ((self
                             (PartialType
                              (PartialStructType
                               ((mk_struct_fields ()) (mk_methods ())
                                (mk_impls ()) (mk_struct_id 66)))))
                            (x (ExprType (Reference (X (TypeN 0)))))))
                          (function_returns (ExprType (Reference (X (TypeN 0)))))))
                        (function_impl
                         (Fn
                          ((Return
                            (Reference (x (ExprType (Reference (X (TypeN 0)))))))))))))))
                   (mk_impls ()) (mk_struct_id 66)))))))))))))
       (structs
        ((70
          ((struct_fields ())
           (struct_methods
            ((id
              ((function_signature
                ((function_params ((self (StructType 70)) (x (StructType 69))))
                 (function_returns (StructType 69))))
               (function_impl (Fn ((Return (Reference (x (StructType 69)))))))))))
           (struct_impls ()) (struct_id 70)))
         (69
          ((struct_fields ()) (struct_methods ()) (struct_impls ())
           (struct_id 69)))
         (67
          ((struct_fields ())
           (struct_methods
            ((id
              ((function_signature
                ((function_params ((self (StructType 67)) (x IntegerType)))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Reference (x IntegerType))))))))))
           (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((must_be_64 (Value (Integer 64))) (must_be_32 (Value (Integer 32)))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ((i (UnionType 67))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Break
                 (Switch
                  ((switch_condition (Reference (i (UnionType 67))))
                   (branches
                    (((branch_ty (StructType 40)) (branch_var vax)
                      (branch_stmt (Block ((Return (Value (Integer 32)))))))
                     ((branch_ty (StructType 39)) (branch_var vax)
                      (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))))
         (Ints (Value (Type (UnionType 67))))))
       (structs ())
       (unions
        ((67
          ((cases
            (((StructType 39) (Discriminator 1))
             ((StructType 40) (Discriminator 0))))
           (union_methods ())
           (union_impls
            (((impl_interface 68)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (StructType 40))))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant ((Reference (v (StructType 40))) 67)))))))))))
             ((impl_interface 69)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (StructType 39))))
                    (function_returns (UnionType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant ((Reference (v (StructType 39))) 67)))))))))))))
           (union_id 67)))))
       (interfaces
        ((69
          ((interface_methods
            ((from
              ((function_params ((from (StructType 39))))
               (function_returns SelfType)))))))
         (68
          ((interface_methods
            ((from
              ((function_params ((from (StructType 40))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((b (Value (Integer 10)))
         (a
          (Value
           (Function
            ((function_signature
              ((function_params ((y IntegerType)))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Return
                 (FunctionCall
                  ((ResolvedReference (left <opaque>))
                   ((Value (Integer 10)) (Reference (y IntegerType)))))))))))))
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
               ((Return
                 (MkFunction
                  ((function_signature
                    ((function_params ((y IntegerType)))
                     (function_returns IntegerType)))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (left <opaque>))
                         ((Reference (x IntegerType))
                          (Reference (y IntegerType)))))))))))))))))))
         (left
          (Value
           (Function
            ((function_signature
              ((function_params ((x IntegerType) (y IntegerType)))
               (function_returns IntegerType)))
             (function_impl (Fn ((Return (Reference (x IntegerType))))))))))))
       (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

let%expect_test "let binding with type" =
  let source = {|
      let a: Int(257) = 1;
      let a: Int(32) = 2;
    |} in
  pp_compile source ;
  [%expect
    {|
    (StructType 66)IntegerType(StructType 66)IntegerType(StructType 40)IntegerType
    (StructType 40)IntegerType(Error
                               (((UnexpectedType (TypeN 0))
                                 (TypeError ((TypeN 0) VoidType))
                                 (FieldNotFoundF slice) (FieldNotFoundF value)
                                 (UnexpectedType VoidType) (FieldNotFoundF slice)
                                 (FieldNotFoundF value) (FieldNotFoundF value)
                                 (UnexpectedType (TypeN 0))
                                 (TypeError ((TypeN 0) VoidType))
                                 (FieldNotFoundF slice) (FieldNotFoundF value)
                                 (FieldNotFoundF slice) (FieldNotFoundF slice)
                                 (FieldNotFoundF value) (FieldNotFoundF value)
                                 (MethodNotFound ((Value (Type (TypeN 0))) new))
                                 (TypeError ((TypeN 0) VoidType))
                                 (FieldNotFoundF slice) (FieldNotFoundF slice)
                                 (UnexpectedType VoidType) (FieldNotFoundF slice)
                                 (FieldNotFoundF value) (FieldNotFoundF value)
                                 (FieldNotFoundF value)
                                 (MethodNotFound ((Value (Type (TypeN 0))) new))
                                 (TypeError ((TypeN 0) VoidType))
                                 (FieldNotFoundF slice) (FieldNotFoundF value)
                                 (FieldNotFoundF slice) (FieldNotFoundF value)
                                 (FieldNotFoundF slice) (FieldNotFoundF slice)
                                 (FieldNotFoundF value) (FieldNotFoundF value)
                                 (MethodNotFound ((Value (Type (TypeN 0))) new))
                                 (TypeError ((TypeN 0) VoidType))
                                 (FieldNotFoundF slice) (FieldNotFoundF value)
                                 (MethodNotFound ((Value (Type (TypeN 0))) new))
                                 (TypeError ((TypeN 0) VoidType)))
                                ((bindings
                                  ((a
                                    (Value
                                     (Struct
                                      ((Value (Type (StructType 40)))
                                       ((value (Value (Integer 2))))))))
                                   (a
                                    (Value
                                     (Struct
                                      ((Value (Type (StructType 66)))
                                       ((value (Value (Integer 1))))))))))
                                 (structs
                                  ((66
                                    ((struct_fields
                                      ((value ((field_type IntegerType)))))
                                     (struct_methods
                                      ((new
                                        ((function_signature
                                          ((function_params ((i IntegerType)))
                                           (function_returns (StructType 66))))
                                         (function_impl
                                          (Fn
                                           ((Return
                                             (Value
                                              (Struct
                                               ((Value (Type (StructType 66)))
                                                ((value
                                                  (Reference (i IntegerType)))))))))))))
                                       (serialize
                                        ((function_signature
                                          ((function_params
                                            ((self (StructType 66))
                                             (builder (StructType 3))))
                                           (function_returns (StructType 3))))
                                         (function_impl
                                          (Fn
                                           ((Return
                                             (FunctionCall
                                              ((ResolvedReference
                                                (serialize_int <opaque>))
                                               ((Reference
                                                 (builder (StructType 3)))
                                                (StructField
                                                 ((Reference
                                                   (self (StructType 66)))
                                                  value IntegerType))
                                                (Value (Integer 257)))))))))))
                                       (deserialize
                                        ((function_signature
                                          ((function_params ((s (StructType 6))))
                                           (function_returns (TypeN 0))))
                                         (function_impl
                                          (Fn
                                           ((Block
                                             ((Let
                                               ((res
                                                 (FunctionCall
                                                  ((ResolvedReference
                                                    (load_int <opaque>))
                                                   ((Reference
                                                     (s (StructType 6)))
                                                    (Value (Integer 257))))))))
                                              (Return
                                               (Value
                                                (Struct
                                                 ((Value (Type VoidType)) ())))))))))))
                                       (from
                                        ((function_signature
                                          ((function_params ((i IntegerType)))
                                           (function_returns (StructType 66))))
                                         (function_impl
                                          (Fn
                                           ((Return
                                             (Value
                                              (Struct
                                               ((Value (Type (StructType 66)))
                                                ((value
                                                  (Reference (i IntegerType)))))))))))))))
                                     (struct_impls
                                      (((impl_interface -1)
                                        (impl_methods
                                         ((serialize
                                           ((function_signature
                                             ((function_params
                                               ((self (StructType 66))
                                                (builder (StructType 3))))
                                              (function_returns (StructType 3))))
                                            (function_impl
                                             (Fn
                                              ((Return
                                                (FunctionCall
                                                 ((ResolvedReference
                                                   (serialize_int <opaque>))
                                                  ((Reference
                                                    (builder (StructType 3)))
                                                   (StructField
                                                    ((Reference
                                                      (self (StructType 66)))
                                                     value IntegerType))
                                                   (Value (Integer 257))))))))))))))
                                       ((impl_interface -3)
                                        (impl_methods
                                         ((deserialize
                                           ((function_signature
                                             ((function_params
                                               ((s (StructType 6))))
                                              (function_returns (TypeN 0))))
                                            (function_impl
                                             (Fn
                                              ((Block
                                                ((Let
                                                  ((res
                                                    (FunctionCall
                                                     ((ResolvedReference
                                                       (load_int <opaque>))
                                                      ((Reference
                                                        (s (StructType 6)))
                                                       (Value (Integer 257))))))))
                                                 (Return
                                                  (Value
                                                   (Struct
                                                    ((Value (Type VoidType)) ()))))))))))))))
                                       ((impl_interface 10)
                                        (impl_methods
                                         ((from
                                           ((function_signature
                                             ((function_params ((i IntegerType)))
                                              (function_returns (StructType 66))))
                                            (function_impl
                                             (Fn
                                              ((Return
                                                (Value
                                                 (Struct
                                                  ((Value (Type (StructType 66)))
                                                   ((value
                                                     (Reference (i IntegerType))))))))))))))))))
                                     (struct_id 66)))))
                                 (type_counter <opaque>)
                                 (memoized_fcalls <opaque>)))) |}]

let%expect_test "let binding with a non-matching type" =
  let source = {|
      let a: Bool = 1;
    |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (TypeError (BoolType IntegerType))
       (TypeError (BoolType IntegerType)))
      ((bindings ((a (Value Void)))) (structs ()) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((must_be_one (Value (Integer 1)))
         (concrete_beeper
          (Value
           (Function
            ((function_signature
              ((function_params ((t (StructType 68))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Return
                 (FunctionCall
                  ((Value
                    (Function
                     ((function_signature
                       ((function_params ((self (StructType 68))))
                        (function_returns IntegerType)))
                      (function_impl (Fn ((Return (Value (Integer 1)))))))))
                   ((Reference (t (StructType 68))))))))))))))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ((T (InterfaceType 66))))
               (function_returns
                (FunctionType
                 ((function_params ((t (Dependent T (InterfaceType 66)))))
                  (function_returns IntegerType))))))
             (function_impl
              (Fn
               ((Return
                 (MkFunction
                  ((function_signature
                    ((function_params
                      ((t (ExprType (Reference (T (InterfaceType 66)))))))
                     (function_returns IntegerType)))
                   (function_impl
                    (Fn
                     ((Return
                       (IntfMethodCall
                        ((intf_instance (Reference (T (InterfaceType 66))))
                         (intf_def 66)
                         (intf_method
                          (beep
                           ((function_params ((self SelfType)))
                            (function_returns IntegerType))))
                         (intf_args
                          ((Reference
                            (t (ExprType (Reference (T (InterfaceType 66))))))))))))))))))))))))
         (BeeperImpl1 (Value (Type (StructType 68))))
         (Beep (Value (Type (InterfaceType 66))))))
       (structs
        ((68
          ((struct_fields ())
           (struct_methods
            ((beep
              ((function_signature
                ((function_params ((self (StructType 68))))
                 (function_returns IntegerType)))
               (function_impl (Fn ((Return (Value (Integer 1))))))))))
           (struct_impls
            (((impl_interface 66)
              (impl_methods
               ((beep
                 ((function_signature
                   ((function_params ((self (StructType 68))))
                    (function_returns IntegerType)))
                  (function_impl (Fn ((Return (Value (Integer 1)))))))))))))
           (struct_id 68)))))
       (interfaces
        ((66
          ((interface_methods
            ((beep
              ((function_params ((self SelfType)))
               (function_returns IntegerType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((x (Value (Integer 2)))
         (test
          (Value
           (Function
            ((function_signature
              ((function_params ((t (StructType 67))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Block
                 ((DestructuringLet
                   ((destructuring_let ((x x) (y y2) (z z)))
                    (destructuring_let_expr (Reference (t (StructType 67))))
                    (destructuring_let_rest false)))
                  (Return (Reference (y2 HoleType))))))))))))
         (T (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields
            ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
             (z ((field_type IntegerType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (MissingField (67 x))
       (MissingField (67 z)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((t (StructType 67))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Block
                 ((DestructuringLet
                   ((destructuring_let ((y y2)))
                    (destructuring_let_expr (Reference (t (StructType 67))))
                    (destructuring_let_rest false)))
                  (Return (Reference (y2 HoleType))))))))))))
         (T (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields
            ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
             (z ((field_type IntegerType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((t (StructType 67))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Block
                 ((DestructuringLet
                   ((destructuring_let ((y y2)))
                    (destructuring_let_expr (Reference (t (StructType 67))))
                    (destructuring_let_rest true)))
                  (Return (Reference (y2 HoleType))))))))))))
         (T (Value (Type (StructType 67))))))
       (structs
        ((67
          ((struct_fields
            ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
             (z ((field_type IntegerType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType))
       (TypeError ((InterfaceType 66) (TypeN 0))))
      ((bindings
        ((a (Value Void)) (Foo (Value (Type (StructType 68))))
         (ExpectedIntf
          (Value
           (Function
            ((function_signature
              ((function_params ((T (InterfaceType 66))))
               (function_returns HoleType)))
             (function_impl (Fn ((Block ()))))))))
         (Intf (Value (Type (InterfaceType 66))))))
       (structs
        ((68
          ((struct_fields ()) (struct_methods ()) (struct_impls ())
           (struct_id 68)))))
       (interfaces ((66 ((interface_methods ()))))) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]

let%expect_test "struct signatures" =
  let source =
    {|
      struct Int2(bits: Integer) {
        val value: Integer
      }
      fn extract_value(n: Integer) {
        fn(x: Int2(n)) -> Integer {  
          x.value
        }
      }
      let five = extract_value(10)(Int(10).new(5));
      let zero = extract_value(20)(Int(20).new(0));
    |}
  in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (UnexpectedType VoidType)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (UnexpectedType (TypeN 0)) (TypeError ((TypeN 0) VoidType))
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF slice) (UnexpectedType VoidType) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF value) (FieldNotFoundF value)
       (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF slice) (FieldNotFoundF slice) (FieldNotFoundF value)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)) (FieldNotFoundF slice)
       (FieldNotFoundF value) (MethodNotFound ((Value (Type (TypeN 0))) new))
       (TypeError ((TypeN 0) VoidType)))
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
                     (StructSig
                      ((st_sig_fields
                        ((value (ResolvedReference (Integer <opaque>))))))))))
                  (function_returns IntegerType))))))
             (function_impl
              (Fn
               ((Return
                 (MkFunction
                  ((function_signature
                    ((function_params
                      ((x
                        (StructSig
                         ((st_sig_fields
                           ((value (ResolvedReference (Integer <opaque>))))))))))
                     (function_returns IntegerType)))
                   (function_impl
                    (Fn
                     ((Return
                       (StructField
                        ((Reference
                          (x
                           (StructSig
                            ((st_sig_fields
                              ((value (ResolvedReference (Integer <opaque>)))))))))
                         value IntegerType))))))))))))))))
         (Int2
          (Value
           (Function
            ((function_signature
              ((function_params ((bits IntegerType)))
               (function_returns
                (StructSig
                 ((st_sig_fields
                   ((value (ResolvedReference (Integer <opaque>))))))))))
             (function_impl
              (Fn
               ((Return
                 (MkStructDef
                  ((mk_struct_fields
                    ((value (ResolvedReference (Integer <opaque>)))))
                   (mk_methods ()) (mk_impls ()) (mk_struct_id 66)))))))))))))
       (structs
        ((68
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 68))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 68)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 68)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 68))) value IntegerType))
                      (Value (Integer 20)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 20))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 68))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 68)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 68)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 68))) value IntegerType))
                         (Value (Integer 20))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 20))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 68))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 68)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 68)))
         (67
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 67))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 67)))
                      ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 67)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 67))) value IntegerType))
                      (Value (Integer 10)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (TypeN 0))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                    (Return (Value (Struct ((Value (Type VoidType)) ())))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 67))))
               (function_impl
                (Fn
                 ((Return
                   (Value
                    (Struct
                     ((Value (Type (StructType 67)))
                      ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 67)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 67))) value IntegerType))
                         (Value (Integer 10))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (TypeN 0))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                       (Return (Value (Struct ((Value (Type VoidType)) ()))))))))))))))
             ((impl_interface 10)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 67))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value
                       (Struct
                        ((Value (Type (StructType 67)))
                         ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 67)))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]
