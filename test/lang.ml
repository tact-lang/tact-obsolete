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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 59))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "binding resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 59))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((UnresolvedIdentifier Int256)
       ((bindings ()) (structs ()) (type_counter <opaque>)
        (memoized_fcalls <opaque>))))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((B (Value (Type (StructType 59)))) (A (Value (Type (StructType 59))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 61))))))
      (structs
       ((61
         ((struct_fields ((t ((field_type (StructType 59))))))
          (struct_methods ()) (struct_impls ()) (struct_id 61)))
        (59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp source
    ~prev_program:
      { (Lang.default_program ()) with
        bindings = ("incr", Value incr_f) :: Lang.default_bindings () } ;
  [%expect
    {|
    (Ok
     ((bindings ((v (Value (Integer 4))))) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>))) |}]

let%expect_test "Tact function evaluation" =
  let source =
    {|
    fn test(i: Int(257)) -> Int(257) {
      i
    }
    let a = test(test(Int(257).new(1)));
  |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Struct (59 ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 59))))
              (function_returns (StructType 59))))
            (function_impl (Fn ((Return (Reference (i (StructType 59)))))))))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "compile-time function evaluation within a function" =
  let source =
    {|
    fn test() {
      let v = incr(incr(incr(1)));
      v
    }
  |}
  in
  pp source
    ~prev_program:
      { (Lang.default_program ()) with
        bindings = ("incr", Value incr_f) :: Lang.default_bindings () } ;
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
              ((Block
                ((Let ((v (Value (Integer 4)))))
                 (Return (ResolvedReference (v <opaque>))))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
       val a: Int(257)
       val b: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
      (Ok
       ((bindings ((MyType (Value (Type (StructType 61))))))
        (structs
         ((61
           ((struct_fields
             ((a ((field_type (StructType 59)))) (b ((field_type BoolType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 61)))
          (59
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 59)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Return
                    (FunctionCall
                     ((ResolvedReference (serialize_int <opaque>))
                      ((Reference (builder (StructType 3)))
                       (StructField
                        ((Reference (self (StructType 59))) value IntegerType))
                       (Value (Integer 257)))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 12))))
                (function_impl
                 (Fn
                  ((Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((ResolvedReference (load_int <opaque>))
                          ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                     (Return
                      (Value
                       (Struct
                        (12
                         ((slice
                           (StructField
                            ((Reference (res (StructType 6))) slice (StructType 7))))
                          (value
                           (Value
                            (Struct
                             (59
                              ((value
                                (StructField
                                 ((Reference (res (StructType 6))) value
                                  IntegerType))))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 8))))
               (impl_methods
                ((serialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((self (StructType 59)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       ((Return
                         (FunctionCall
                          ((ResolvedReference (serialize_int <opaque>))
                           ((Reference (builder (StructType 3)))
                            (StructField
                             ((Reference (self (StructType 59))) value IntegerType))
                            (Value (Integer 257))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 10))))
               (impl_methods
                ((deserialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 12))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Let
                           ((res
                             (FunctionCall
                              ((ResolvedReference (load_int <opaque>))
                               ((Reference (s (StructType 7)))
                                (Value (Integer 257))))))))
                          (Return
                           (Value
                            (Struct
                             (12
                              ((slice
                                (StructField
                                 ((Reference (res (StructType 6))) slice
                                  (StructType 7))))
                               (value
                                (Value
                                 (Struct
                                  (59
                                   ((value
                                     (StructField
                                      ((Reference (res (StructType 6))) value
                                       IntegerType)))))))))))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 59))))
                     (function_impl
                      (Fn
                       ((Return
                         (Value
                          (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
            (struct_id 59)))))
        (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      val a: Int(257)
      val a: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
    (Error
     (((DuplicateField
        (a
         ((mk_struct_fields
           ((a (Value (Type (StructType 59)))) (a (Value (Type BoolType)))))
          (mk_methods ()) (mk_impls ()) (mk_struct_id -1))))
       ((bindings ((MyType (Value (Type (StructType 61))))))
        (structs
         ((61
           ((struct_fields
             ((a ((field_type (StructType 59)))) (a ((field_type BoolType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 61)))
          (59
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 59)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Return
                    (FunctionCall
                     ((ResolvedReference (serialize_int <opaque>))
                      ((Reference (builder (StructType 3)))
                       (StructField
                        ((Reference (self (StructType 59))) value IntegerType))
                       (Value (Integer 257)))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 12))))
                (function_impl
                 (Fn
                  ((Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((ResolvedReference (load_int <opaque>))
                          ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                     (Return
                      (Value
                       (Struct
                        (12
                         ((slice
                           (StructField
                            ((Reference (res (StructType 6))) slice
                             (StructType 7))))
                          (value
                           (Value
                            (Struct
                             (59
                              ((value
                                (StructField
                                 ((Reference (res (StructType 6))) value
                                  IntegerType))))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 8))))
               (impl_methods
                ((serialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((self (StructType 59)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       ((Return
                         (FunctionCall
                          ((ResolvedReference (serialize_int <opaque>))
                           ((Reference (builder (StructType 3)))
                            (StructField
                             ((Reference (self (StructType 59))) value
                              IntegerType))
                            (Value (Integer 257))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 10))))
               (impl_methods
                ((deserialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 12))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Let
                           ((res
                             (FunctionCall
                              ((ResolvedReference (load_int <opaque>))
                               ((Reference (s (StructType 7)))
                                (Value (Integer 257))))))))
                          (Return
                           (Value
                            (Struct
                             (12
                              ((slice
                                (StructField
                                 ((Reference (res (StructType 6))) slice
                                  (StructType 7))))
                               (value
                                (Value
                                 (Struct
                                  (59
                                   ((value
                                     (StructField
                                      ((Reference (res (StructType 6))) value
                                       IntegerType)))))))))))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 59))))
                     (function_impl
                      (Fn
                       ((Return
                         (Value
                          (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
            (struct_id 59)))))
        (type_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T(A: Type) { val a: A }
      let TA = T(Int(257));
   |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((TA (Value (Type (StructType 61))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ((A (TypeN 0)))) (function_returns (TypeN 0))))
            (function_impl
             (Fn
              ((Expr
                (MkStructDef
                 ((mk_struct_fields ((a (Reference (A (TypeN 0))))))
                  (mk_methods ()) (mk_impls ()) (mk_struct_id 59)))))))))))))
      (structs
       ((61
         ((struct_fields ((a ((field_type (StructType 60))))))
          (struct_methods ()) (struct_impls ()) (struct_id 61)))
        (60
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 60))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (60 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 60)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 60))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (60
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 60))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (60 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 60)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 60))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (60
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (60 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp source ;
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
              (function_impl (Fn ((Return (Value (Integer 1))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b (Value (Struct (59 ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 59))))
              (function_returns (StructType 59))))
            (function_impl
             (Fn
              ((Block
                ((Let ((a (Reference (i (StructType 59))))))
                 (Return (Reference (a (StructType 59)))))))))))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((f
         (Value
          (Function
           ((function_signature
             ((function_params ((x (StructType 59))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (x (StructType 59)))
                       (Reference (x (StructType 59)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (a (StructType 59)))
                       (Reference (a (StructType 59))))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 59)) (i_ (StructType 59))))
              (function_returns (StructType 59))))
            (function_impl (Fn ((Return (Reference (i (StructType 59)))))))))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
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
            (function_impl (Fn ((Return (ResolvedReference (i <opaque>))))))))))
        (i (Value (Integer 1)))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1))) (foo (Value (Struct (60 ()))))
        (Foo (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 60)) (i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((res (Value (Integer 1))) (Foo (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((Foo (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 60))))
                (function_returns (StructType 60))))
              (function_impl (Fn ((Return (Reference (self (StructType 60)))))))))))
          (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
      (Ok
       ((bindings
         ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 60))))
          (make_foo
           (Value
            (Function
             ((function_signature
               ((function_params ((foo (UnionType 60))))
                (function_returns (UnionType 60))))
              (function_impl (Fn ((Return (Reference (foo (UnionType 60)))))))))))
          (Foo (Value (Type (UnionType 60))))))
        (structs ())
        (unions
         ((60
           ((cases ((BoolType (Discriminator 0))))
            (union_methods
             ((bar
               ((function_signature
                 ((function_params ((self (UnionType 60)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 61))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v (ExprType (Value (Type BoolType))))))
                       (function_returns (UnionType 60))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Value (Type BoolType))))) 60)))))))))))))))
            (union_id 60)))))
        (interfaces
         ((61
           ((interface_methods
             ((from
               ((function_params ((from BoolType))) (function_returns SelfType)))))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)))
      |}]

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
  pp source ;
  [%expect
    {|
      (Ok
       ((bindings
         ((res (Value (Integer 1)))
          (make_foo
           (Value
            (Function
             ((function_signature
               ((function_params ((foo (UnionType 60))))
                (function_returns (UnionType 60))))
              (function_impl (Fn ((Return (Reference (foo (UnionType 60)))))))))))
          (Foo (Value (Type (UnionType 60))))))
        (structs ())
        (unions
         ((60
           ((cases ((BoolType (Discriminator 0))))
            (union_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 61))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v (ExprType (Value (Type BoolType))))))
                       (function_returns (UnionType 60))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Value (Type BoolType))))) 60)))))))))))))))
            (union_id 60)))))
        (interfaces
         ((61
           ((interface_methods
             ((from
               ((function_params ((from BoolType))) (function_returns SelfType)))))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)))
      |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((t
         (Value (Struct (60 ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError ((StructType 45) (StructType 46)))
       ((bindings
         ((foo
           (Value
            (Function
             ((function_signature
               ((function_params ((i (StructType 46))))
                (function_returns (StructType 45))))
              (function_impl (Fn ((Return (Reference (i (StructType 46)))))))))))))
        (structs ())
        (interfaces
         ((59
           ((interface_methods
             ((from
               ((function_params ((from (StructType 46))))
                (function_returns SelfType)))))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

let%expect_test "type inference" =
  let source = {|
      fn foo(i: Integer) { return i; }
    |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((foo
         (Value
          (Function
           ((function_signature
             ((function_params ((i IntegerType))) (function_returns IntegerType)))
            (function_impl (Fn ((Return (Reference (i IntegerType))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "scope doesn't leak bindings" =
  let source = {|
    {
     let a = 1;
    }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ()) (structs ()) (type_counter <opaque>)
      (memoized_fcalls <opaque>))) |}]

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
  pp source ;
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
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "type check error" =
  let source =
    {|
      {
        fn foo(x: Int(99)) { return x; }

        let a = foo(Int(10).new(1))
      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError ((StructType 59) (StructType 60)))
       ((bindings ())
        (structs
         ((60
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 60))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (60 ((value (Reference (i IntegerType)))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 60)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Return
                    (FunctionCall
                     ((ResolvedReference (serialize_int <opaque>))
                      ((Reference (builder (StructType 3)))
                       (StructField
                        ((Reference (self (StructType 60))) value IntegerType))
                       (Value (Integer 10)))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 12))))
                (function_impl
                 (Fn
                  ((Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((ResolvedReference (load_int <opaque>))
                          ((Reference (s (StructType 7))) (Value (Integer 10))))))))
                     (Return
                      (Value
                       (Struct
                        (12
                         ((slice
                           (StructField
                            ((Reference (res (StructType 6))) slice
                             (StructType 7))))
                          (value
                           (Value
                            (Struct
                             (60
                              ((value
                                (StructField
                                 ((Reference (res (StructType 6))) value
                                  IntegerType))))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 60))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (60 ((value (Reference (i IntegerType)))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 8))))
               (impl_methods
                ((serialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((self (StructType 60)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       ((Return
                         (FunctionCall
                          ((ResolvedReference (serialize_int <opaque>))
                           ((Reference (builder (StructType 3)))
                            (StructField
                             ((Reference (self (StructType 60))) value
                              IntegerType))
                            (Value (Integer 10))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 10))))
               (impl_methods
                ((deserialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 12))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Let
                           ((res
                             (FunctionCall
                              ((ResolvedReference (load_int <opaque>))
                               ((Reference (s (StructType 7)))
                                (Value (Integer 10))))))))
                          (Return
                           (Value
                            (Struct
                             (12
                              ((slice
                                (StructField
                                 ((Reference (res (StructType 6))) slice
                                  (StructType 7))))
                               (value
                                (Value
                                 (Struct
                                  (60
                                   ((value
                                     (StructField
                                      ((Reference (res (StructType 6))) value
                                       IntegerType)))))))))))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 60))))
                     (function_impl
                      (Fn
                       ((Return
                         (Value
                          (Struct (60 ((value (Reference (i IntegerType))))))))))))))))))))
            (struct_id 60)))
          (59
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 59)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Return
                    (FunctionCall
                     ((ResolvedReference (serialize_int <opaque>))
                      ((Reference (builder (StructType 3)))
                       (StructField
                        ((Reference (self (StructType 59))) value IntegerType))
                       (Value (Integer 99)))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 12))))
                (function_impl
                 (Fn
                  ((Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((ResolvedReference (load_int <opaque>))
                          ((Reference (s (StructType 7))) (Value (Integer 99))))))))
                     (Return
                      (Value
                       (Struct
                        (12
                         ((slice
                           (StructField
                            ((Reference (res (StructType 6))) slice
                             (StructType 7))))
                          (value
                           (Value
                            (Struct
                             (59
                              ((value
                                (StructField
                                 ((Reference (res (StructType 6))) value
                                  IntegerType))))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 8))))
               (impl_methods
                ((serialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((self (StructType 59)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       ((Return
                         (FunctionCall
                          ((ResolvedReference (serialize_int <opaque>))
                           ((Reference (builder (StructType 3)))
                            (StructField
                             ((Reference (self (StructType 59))) value
                              IntegerType))
                            (Value (Integer 99))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 10))))
               (impl_methods
                ((deserialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 12))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Let
                           ((res
                             (FunctionCall
                              ((ResolvedReference (load_int <opaque>))
                               ((Reference (s (StructType 7)))
                                (Value (Integer 99))))))))
                          (Return
                           (Value
                            (Struct
                             (12
                              ((slice
                                (StructField
                                 ((Reference (res (StructType 6))) slice
                                  (StructType 7))))
                               (value
                                (Value
                                 (Struct
                                  (59
                                   ((value
                                     (StructField
                                      ((Reference (res (StructType 6))) value
                                       IntegerType)))))))))))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 59))))
                     (function_impl
                      (Fn
                       ((Return
                         (Value
                          (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
            (struct_id 59)))))
        (interfaces
         ((61
           ((interface_methods
             ((from
               ((function_params ((from (StructType 60))))
                (function_returns SelfType)))))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

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
  pp source ~prev_program:(add_bin_op_intf (Lang.default_program ())) ;
  [%expect
    {|
    (Ok
     ((bindings
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 60))))))
      (structs
       ((60
         ((struct_fields ())
          (struct_methods
           ((op
             ((function_signature
               ((function_params ((left IntegerType) (right IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (left IntegerType))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -10))))
             (impl_methods
              ((op
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((left IntegerType) (right IntegerType)))
                     (function_returns IntegerType)))
                   (function_impl (Fn ((Return (Reference (left IntegerType)))))))))))))))
          (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((empty (Value (Struct (61 ())))) (Empty (Value (Type (StructType 61))))
        (Make (Value (Type (InterfaceType 59))))))
      (structs
       ((61
         ((struct_fields ())
          (struct_methods
           ((new
             ((function_signature
               ((function_params ()) (function_returns (StructType 61))))
              (function_impl (Fn ((Return (Value (Struct (61 ())))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 59))))
             (impl_methods
              ((new
                (Value
                 (Function
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 61))))
                   (function_impl (Fn ((Return (Value (Struct (61 ()))))))))))))))))
          (struct_id 61)))))
      (interfaces
       ((59
         ((interface_methods
           ((new ((function_params ()) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "serializer inner struct" =
  let source =
    {|
      struct Inner { val x: Int(32) }
      struct Outer { val y: Int(32) val z: Inner }
      let serialize_outer = serializer(Outer);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((serialize_outer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 62)) (b (StructType 3))))
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
                            ((self (StructType 46)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 46))) value
                                  IntegerType))
                                (Value (Integer 32))))))))))))
                      ((StructField
                        ((Reference (self (StructType 62))) y (StructType 46)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (Outer (Value (Type (StructType 62))))
        (Inner (Value (Type (StructType 60))))))
      (structs
       ((62
         ((struct_fields
           ((y ((field_type (StructType 46))))
            (z ((field_type (StructType 60))))))
          (struct_methods ()) (struct_impls ()) (struct_id 62)))
        (60
         ((struct_fields ((x ((field_type (StructType 46))))))
          (struct_methods ()) (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "reference resolving in inner functions" =
  let source =
    {|
      fn foo(X: Type) {
        fn(x: X) -> X { x }
      }
      let one = foo(Integer)(1);
    |}
  in
  pp source ;
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
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
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
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "TypeN" =
  let source =
    {|
      fn id(X: Type) { X }
      let must_fail = id(Type);
    |}
  in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError ((TypeN 0) (TypeN 1)))
       ((bindings
         ((must_fail (Value Void))
          (id
           (Value
            (Function
             ((function_signature
               ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
              (function_impl (Fn ((Return (Reference (X (TypeN 0)))))))))))))
        (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b
         (Value (UnionVariant ((Struct (46 ((value (Value (Integer 1)))))) 60))))
        (a (Value (UnionVariant ((Integer 10) 60))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 60))))
              (function_returns (UnionType 60))))
            (function_impl (Fn ((Return (Reference (value (UnionType 60)))))))))))
        (Uni (Value (Type (UnionType 60))))))
      (structs ())
      (unions
       ((60
         ((cases
           (((StructType 46) (Discriminator 0)) (IntegerType (Discriminator 1))))
          (union_methods ())
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type IntegerType))))))
                     (function_returns (UnionType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference (v (ExprType (Value (Type IntegerType)))))
                         60)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 61))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 46)))))))
                     (function_returns (UnionType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 46))))))
                         60)))))))))))))))
          (union_id 60)))))
      (interfaces
       ((61
         ((interface_methods
           ((from
             ((function_params ((from (StructType 46))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Error
     (((DuplicateVariant IntegerType)
       ((bindings
         ((b (Value (Type (UnionType 62)))) (a (Value (Type (UnionType 60))))
          (Test
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0))))
                (function_returns
                 (InvalidType
                  (MkUnionDef
                   ((mk_cases
                     ((ResolvedReference (Integer <opaque>))
                      (Value (Type (Dependent T (TypeN 0))))))
                    (mk_union_methods ())
                    (mk_union_impls
                     (((impl_interface
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((T (TypeN 0))))
                               (function_returns HoleType)))
                             (function_impl (BuiltinFn (<fun> <opaque>))))))
                          ((Value (Type IntegerType))))))
                       (impl_methods
                        ((from
                          (Value
                           (Function
                            ((function_signature
                              ((function_params ((v IntegerType)))
                               (function_returns (UnionType 59))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Value
                                    (Type (ExprType (Reference (v IntegerType)))))
                                   59)))))))))))))
                      ((impl_interface
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((T (TypeN 0))))
                               (function_returns HoleType)))
                             (function_impl (BuiltinFn (<fun> <opaque>))))))
                          ((Value (Type (Dependent T (TypeN 0))))))))
                       (impl_methods
                        ((from
                          (Value
                           (Function
                            ((function_signature
                              ((function_params ((v (Dependent T (TypeN 0)))))
                               (function_returns (UnionType 59))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Value
                                    (Type
                                     (ExprType
                                      (Reference
                                       (v (ExprType (Reference (T (TypeN 0)))))))))
                                   59)))))))))))))))
                    (mk_union_id 59)))))))
              (function_impl
               (Fn
                ((Return
                  (MkUnionDef
                   ((mk_cases
                     ((ResolvedReference (Integer <opaque>))
                      (Reference (T (TypeN 0)))))
                    (mk_union_methods ())
                    (mk_union_impls
                     (((impl_interface
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((T (TypeN 0))))
                               (function_returns HoleType)))
                             (function_impl (BuiltinFn (<fun> <opaque>))))))
                          ((Value (Type IntegerType))))))
                       (impl_methods
                        ((from
                          (Value
                           (Function
                            ((function_signature
                              ((function_params ((v IntegerType)))
                               (function_returns (UnionType 59))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Reference (v IntegerType)) 59)))))))))))))
                      ((impl_interface
                        (FunctionCall
                         ((Value
                           (Function
                            ((function_signature
                              ((function_params ((T (TypeN 0))))
                               (function_returns HoleType)))
                             (function_impl (BuiltinFn (<fun> <opaque>))))))
                          ((Value (Type (ExprType (Reference (T (TypeN 0))))))))))
                       (impl_methods
                        ((from
                          (Value
                           (Function
                            ((function_signature
                              ((function_params
                                ((v (ExprType (Reference (T (TypeN 0)))))))
                               (function_returns (UnionType 59))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Reference
                                    (v (ExprType (Reference (T (TypeN 0))))))
                                   59)))))))))))))))
                    (mk_union_id 59)))))))))))))
        (structs ())
        (unions
         ((62
           ((cases ((IntegerType (Discriminator 0)))) (union_methods ())
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v IntegerType)))
                       (function_returns (UnionType 62))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant ((Reference (v IntegerType)) 62)))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((v (ExprType (Reference (T (TypeN 0)))))))
                       (function_returns (UnionType 62))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Reference (T (TypeN 0))))))
                           62)))))))))))))))
            (union_id 62)))
          (60
           ((cases
             (((BuiltinType Builder) (Discriminator 0))
              (IntegerType (Discriminator 1))))
            (union_methods ())
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v IntegerType)))
                       (function_returns (UnionType 60))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant ((Reference (v IntegerType)) 60)))))))))))))
              ((impl_interface (Value (Type (InterfaceType 61))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((v (ExprType (Reference (T (TypeN 0)))))))
                       (function_returns (UnionType 60))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Reference (T (TypeN 0))))))
                           60)))))))))))))))
            (union_id 60)))))
        (interfaces
         ((61
           ((interface_methods
             ((from
               ((function_params ((from (BuiltinType Builder))))
                (function_returns SelfType)))))))))
        (type_counter <opaque>) (memoized_fcalls <opaque>))))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((Test (Value (Type (UnionType 61))))))
      (structs
       ((59
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 59)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 59))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 7))))
                (function_returns (StructType 12))))
              (function_impl
               (Fn
                ((Block
                  ((Let
                    ((res
                      (FunctionCall
                       ((ResolvedReference (load_int <opaque>))
                        ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                   (Return
                    (Value
                     (Struct
                      (12
                       ((slice
                         (StructField
                          ((Reference (res (StructType 6))) slice (StructType 7))))
                        (value
                         (Value
                          (Struct
                           (59
                            ((value
                              (StructField
                               ((Reference (res (StructType 6))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 59))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 8))))
             (impl_methods
              ((serialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((self (StructType 59)) (builder (StructType 3))))
                     (function_returns (StructType 3))))
                   (function_impl
                    (Fn
                     ((Return
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 59))) value IntegerType))
                          (Value (Integer 257))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 10))))
             (impl_methods
              ((deserialize
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((s (StructType 7))))
                     (function_returns (StructType 12))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Let
                         ((res
                           (FunctionCall
                            ((ResolvedReference (load_int <opaque>))
                             ((Reference (s (StructType 7)))
                              (Value (Integer 257))))))))
                        (Return
                         (Value
                          (Struct
                           (12
                            ((slice
                              (StructField
                               ((Reference (res (StructType 6))) slice
                                (StructType 7))))
                             (value
                              (Value
                               (Struct
                                (59
                                 ((value
                                   (StructField
                                    ((Reference (res (StructType 6))) value
                                     IntegerType)))))))))))))))))))))))))
            ((impl_interface (Value (Type (InterfaceType 13))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 59))))
                   (function_impl
                    (Fn
                     ((Return
                       (Value
                        (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
          (struct_id 59)))))
      (unions
       ((61
         ((cases
           (((StructType 45) (Discriminator 0))
            ((StructType 59) (Discriminator 1))))
          (union_methods
           ((id
             ((function_signature
               ((function_params ((self (UnionType 61))))
                (function_returns (UnionType 61))))
              (function_impl (Fn ((Return (Reference (self (UnionType 61)))))))))))
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 62))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 59)))))))
                     (function_returns (UnionType 61))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 59))))))
                         61)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 63))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 45)))))))
                     (function_returns (UnionType 61))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 45))))))
                         61)))))))))))))))
          (union_id 61)))))
      (interfaces
       ((63
         ((interface_methods
           ((from
             ((function_params ((from (StructType 45))))
              (function_returns SelfType)))))))
        (62
         ((interface_methods
           ((from
             ((function_params ((from (StructType 59))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((y (Value (Struct (62 ())))) (foo_empty (Value (Struct (63 ()))))
        (Empty (Value (Type (StructType 62)))) (x (Value (Integer 10)))
        (foo (Value (Struct (60 ()))))
        (Foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
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
                          ((self (StructType 59))
                           (x (ExprType (Reference (X (TypeN 0)))))))
                         (function_returns (ExprType (Reference (X (TypeN 0)))))))
                       (function_impl
                        (Fn
                         ((Return
                           (Reference (x (ExprType (Reference (X (TypeN 0)))))))))))))))
                  (mk_impls ()) (mk_struct_id 59)))))))))))))
      (structs
       ((63
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 63)) (x (StructType 62))))
                (function_returns (StructType 62))))
              (function_impl (Fn ((Return (Reference (x (StructType 62)))))))))))
          (struct_impls ()) (struct_id 63)))
        (62
         ((struct_fields ()) (struct_methods ()) (struct_impls ())
          (struct_id 62)))
        (60
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 60)) (x IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (x IntegerType))))))))))
          (struct_impls ()) (struct_id 60)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((must_be_64 (Value (Integer 64))) (must_be_32 (Value (Integer 32)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (UnionType 60))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Break
                (Switch
                 ((switch_condition (Reference (i (UnionType 60))))
                  (branches
                   (((branch_ty (StructType 46)) (branch_var vax)
                     (branch_stmt (Block ((Return (Value (Integer 32)))))))
                    ((branch_ty (StructType 45)) (branch_var vax)
                     (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))))
        (Ints (Value (Type (UnionType 60))))))
      (structs ())
      (unions
       ((60
         ((cases
           (((StructType 45) (Discriminator 0))
            ((StructType 46) (Discriminator 1))))
          (union_methods ())
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 61))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 46)))))))
                     (function_returns (UnionType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 46))))))
                         60)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 62))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 45)))))))
                     (function_returns (UnionType 60))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 45))))))
                         60)))))))))))))))
          (union_id 60)))))
      (interfaces
       ((62
         ((interface_methods
           ((from
             ((function_params ((from (StructType 45))))
              (function_returns SelfType)))))))
        (61
         ((interface_methods
           ((from
             ((function_params ((from (StructType 46))))
              (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
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
                        ((Reference (x IntegerType)) (Reference (y IntegerType)))))))))))))))))))
        (left
         (Value
          (Function
           ((function_signature
             ((function_params ((x IntegerType) (y IntegerType)))
              (function_returns IntegerType)))
            (function_impl (Fn ((Return (Reference (x IntegerType))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "let binding with type" =
  let source = {|
      let a: Int(257) = 1;
      let a: Int(32) = 2;
    |} in
  pp source ;
  [%expect
    {|
      (Ok
       ((bindings
         ((a (Value (Struct (46 ((value (Value (Integer 2))))))))
          (a (Value (Struct (59 ((value (Value (Integer 1))))))))))
        (structs
         ((59
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 59)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Return
                    (FunctionCall
                     ((ResolvedReference (serialize_int <opaque>))
                      ((Reference (builder (StructType 3)))
                       (StructField
                        ((Reference (self (StructType 59))) value IntegerType))
                       (Value (Integer 257)))))))))))
              (deserialize
               ((function_signature
                 ((function_params ((s (StructType 7))))
                  (function_returns (StructType 12))))
                (function_impl
                 (Fn
                  ((Block
                    ((Let
                      ((res
                        (FunctionCall
                         ((ResolvedReference (load_int <opaque>))
                          ((Reference (s (StructType 7))) (Value (Integer 257))))))))
                     (Return
                      (Value
                       (Struct
                        (12
                         ((slice
                           (StructField
                            ((Reference (res (StructType 6))) slice (StructType 7))))
                          (value
                           (Value
                            (Struct
                             (59
                              ((value
                                (StructField
                                 ((Reference (res (StructType 6))) value
                                  IntegerType))))))))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 59))))
                (function_impl
                 (Fn
                  ((Return
                    (Value (Struct (59 ((value (Reference (i IntegerType)))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 8))))
               (impl_methods
                ((serialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((self (StructType 59)) (builder (StructType 3))))
                       (function_returns (StructType 3))))
                     (function_impl
                      (Fn
                       ((Return
                         (FunctionCall
                          ((ResolvedReference (serialize_int <opaque>))
                           ((Reference (builder (StructType 3)))
                            (StructField
                             ((Reference (self (StructType 59))) value IntegerType))
                            (Value (Integer 257))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 10))))
               (impl_methods
                ((deserialize
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((s (StructType 7))))
                       (function_returns (StructType 12))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Let
                           ((res
                             (FunctionCall
                              ((ResolvedReference (load_int <opaque>))
                               ((Reference (s (StructType 7)))
                                (Value (Integer 257))))))))
                          (Return
                           (Value
                            (Struct
                             (12
                              ((slice
                                (StructField
                                 ((Reference (res (StructType 6))) slice
                                  (StructType 7))))
                               (value
                                (Value
                                 (Struct
                                  (59
                                   ((value
                                     (StructField
                                      ((Reference (res (StructType 6))) value
                                       IntegerType)))))))))))))))))))))))))
              ((impl_interface (Value (Type (InterfaceType 13))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 59))))
                     (function_impl
                      (Fn
                       ((Return
                         (Value
                          (Struct (59 ((value (Reference (i IntegerType))))))))))))))))))))
            (struct_id 59)))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)))
      |}]

let%expect_test "let binding with a non-matching type" =
  let source = {|
      let a: Bool = 1;
    |} in
  pp source ;
  [%expect
    {|
      (Error
       (((TypeError (BoolType IntegerType))
         ((bindings ((a (Value Void)))) (structs ()) (type_counter <opaque>)
          (memoized_fcalls <opaque>)))
        ((TypeError (BoolType IntegerType))
         ((bindings ((a (Value Void)))) (structs ()) (type_counter <opaque>)
          (memoized_fcalls <opaque>)))))
      |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((must_be_one (Value (Integer 1)))
        (concrete_beeper
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 61))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Return
                (FunctionCall
                 ((Value
                   (Function
                    ((function_signature
                      ((function_params ((self (StructType 61))))
                       (function_returns IntegerType)))
                     (function_impl (Fn ((Return (Value (Integer 1)))))))))
                  ((Reference (t (StructType 61))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 59))))
              (function_returns
               (FunctionType
                ((function_params ((t (Dependent T (InterfaceType 59)))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              ((Return
                (MkFunction
                 ((function_signature
                   ((function_params
                     ((t (ExprType (Reference (T (InterfaceType 59)))))))
                    (function_returns IntegerType)))
                  (function_impl
                   (Fn
                    ((Return
                      (IntfMethodCall
                       ((intf_instance (Reference (T (InterfaceType 59))))
                        (intf_def 59)
                        (intf_method
                         (beep
                          ((function_params ((self SelfType)))
                           (function_returns IntegerType))))
                        (intf_args
                         ((Reference
                           (t (ExprType (Reference (T (InterfaceType 59))))))))))))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 61))))
        (Beep (Value (Type (InterfaceType 59))))))
      (structs
       ((61
         ((struct_fields ())
          (struct_methods
           ((beep
             ((function_signature
               ((function_params ((self (StructType 61))))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Value (Integer 1))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 59))))
             (impl_methods
              ((beep
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((self (StructType 61))))
                     (function_returns IntegerType)))
                   (function_impl (Fn ((Return (Value (Integer 1)))))))))))))))
          (struct_id 61)))))
      (interfaces
       ((59
         ((interface_methods
           ((beep
             ((function_params ((self SelfType))) (function_returns IntegerType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((x (Value (Integer 2)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 54))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((DestructuringLet
                  ((destructuring_let ((x x) (y y2) (z z)))
                   (destructuring_let_expr (Reference (t (StructType 54))))
                   (destructuring_let_rest false)))
                 (Return (Reference (y2 HoleType))))))))))))
        (T (Value (Type (StructType 54))))))
      (structs
       ((54
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 54)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)))
 |}]

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
  pp source ;
  [%expect
    {|
    (Error
     (((MissingField (54 x))
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ((t (StructType 54))))
                (function_returns IntegerType)))
              (function_impl
               (Fn
                ((Block
                  ((DestructuringLet
                    ((destructuring_let ((y y2)))
                     (destructuring_let_expr (Reference (t (StructType 54))))
                     (destructuring_let_rest false)))
                   (Return (Reference (y2 HoleType))))))))))))
          (T (Value (Type (StructType 54))))))
        (structs
         ((54
           ((struct_fields
             ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
              (z ((field_type IntegerType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 54)))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)))
      ((MissingField (54 z))
       ((bindings
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params ((t (StructType 54))))
                (function_returns IntegerType)))
              (function_impl
               (Fn
                ((Block
                  ((DestructuringLet
                    ((destructuring_let ((y y2)))
                     (destructuring_let_expr (Reference (t (StructType 54))))
                     (destructuring_let_rest false)))
                   (Return (Reference (y2 HoleType))))))))))))
          (T (Value (Type (StructType 54))))))
        (structs
         ((54
           ((struct_fields
             ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
              (z ((field_type IntegerType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 54)))))
        (type_counter <opaque>) (memoized_fcalls <opaque>)))))
 |}]

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
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 54))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((DestructuringLet
                  ((destructuring_let ((y y2)))
                   (destructuring_let_expr (Reference (t (StructType 54))))
                   (destructuring_let_rest true)))
                 (Return (Reference (y2 HoleType))))))))))))
        (T (Value (Type (StructType 54))))))
      (structs
       ((54
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 54)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>)))
 |}]
