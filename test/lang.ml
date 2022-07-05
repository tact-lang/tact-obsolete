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
    (Ok
     ((bindings ((T (Value (Type (StructType 72))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "binding resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 72))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((UnresolvedIdentifier Int256))
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
    (Ok
     ((bindings
       ((B (Value (Type (StructType 72)))) (A (Value (Type (StructType 72))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 74))))))
      (structs
       ((74
         ((struct_fields ((t ((field_type (StructType 72))))))
          (struct_methods ()) (struct_impls ()) (struct_id 74)))
        (72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Struct (72 ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 72))))
              (function_returns (StructType 72))))
            (function_impl (Fn ((Return (Reference (i (StructType 72)))))))))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
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
  pp_compile source
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((MyType (Value (Type (StructType 74))))))
      (structs
       ((74
         ((struct_fields
           ((a ((field_type (StructType 72)))) (b ((field_type BoolType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 74)))
        (72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
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
  pp_compile source ;
  [%expect
    {|
    (Error
     (((DuplicateField
        (a
         ((mk_struct_fields
           ((a (Value (Type (StructType 72)))) (a (Value (Type BoolType)))))
          (mk_methods ()) (mk_impls ()) (mk_struct_id -1)))))
      ((bindings ((MyType (Value (Type (StructType 74))))))
       (structs
        ((74
          ((struct_fields
            ((a ((field_type (StructType 72)))) (a ((field_type BoolType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 74)))
         (72
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 72))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 72)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 72))) value IntegerType))
                      (Value (Integer 257)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (StructType 10))))
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
                       (10
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            (72
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType))))))))))))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 72))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 72)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 72))) value IntegerType))
                         (Value (Integer 257))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 10))))
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
                          (10
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               (72
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType)))))))))))))))))))))))
             ((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 72))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 72)))))
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
    (Ok
     ((bindings
       ((TA (Value (Type (StructType 74))))
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
                  (mk_methods ()) (mk_impls ()) (mk_struct_id 72)))))))))))))
      (structs
       ((74
         ((struct_fields ((a ((field_type (StructType 73))))))
          (struct_methods ()) (struct_impls ()) (struct_id 74)))
        (73
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 73))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (73 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 73)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 73))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (73
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 73))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (73 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 73)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 73))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (73
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 73))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (73 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 73)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b (Value (Struct (72 ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 72))))
              (function_returns (StructType 72))))
            (function_impl
             (Fn
              ((Block
                ((Let ((a (Reference (i (StructType 72))))))
                 (Return (Reference (a (StructType 72)))))))))))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((f
         (Value
          (Function
           ((function_signature
             ((function_params ((x (StructType 72))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (x (StructType 72)))
                       (Reference (x (StructType 72)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (a (StructType 72)))
                       (Reference (a (StructType 72))))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 72)) (i_ (StructType 72))))
              (function_returns (StructType 72))))
            (function_impl (Fn ((Return (Reference (i (StructType 72)))))))))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1))) (foo (Value (Struct (73 ()))))
        (Foo (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 73)) (i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (struct_impls ()) (struct_id 73)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((res (Value (Integer 1))) (Foo (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (struct_impls ()) (struct_id 73)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings ((Foo (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 73))))
                (function_returns (StructType 73))))
              (function_impl (Fn ((Return (Reference (self (StructType 73)))))))))))
          (struct_impls ()) (struct_id 73)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 73))))
        (make_foo
         (Value
          (Function
           ((function_signature
             ((function_params ((foo (UnionType 73))))
              (function_returns (UnionType 73))))
            (function_impl (Fn ((Return (Reference (foo (UnionType 73)))))))))))
        (Foo (Value (Type (UnionType 73))))))
      (structs ())
      (unions
       ((73
         ((cases ((BoolType (Discriminator 0))))
          (union_methods
           ((bar
             ((function_signature
               ((function_params ((self (UnionType 73)) (i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (union_impls
           (((impl_interface 74)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v BoolType)))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return (MakeUnionVariant ((Reference (v BoolType)) 73)))))))))))))
          (union_id 73)))))
      (interfaces
       ((74
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
             ((function_params ((foo (UnionType 73))))
              (function_returns (UnionType 73))))
            (function_impl (Fn ((Return (Reference (foo (UnionType 73)))))))))))
        (Foo (Value (Type (UnionType 73))))))
      (structs ())
      (unions
       ((73
         ((cases ((BoolType (Discriminator 0))))
          (union_methods
           ((bar
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (i IntegerType))))))))))
          (union_impls
           (((impl_interface 74)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v BoolType)))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return (MakeUnionVariant ((Reference (v BoolType)) 73)))))))))))))
          (union_id 73)))))
      (interfaces
       ((74
         ((interface_methods
           ((from
             ((function_params ((from BoolType))) (function_returns SelfType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
         (Value (Struct (73 ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 73)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((TypeError ((StructType 43) (StructType 44))))
      ((bindings
        ((foo
          (Value
           (Function
            ((function_signature
              ((function_params ((i (StructType 44))))
               (function_returns (StructType 43))))
             (function_impl (Fn ((Return (Reference (i (StructType 44)))))))))))))
       (structs ())
       (interfaces
        ((72
          ((interface_methods
            ((from
              ((function_params ((from (StructType 44))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
            (function_impl (Fn ((Return (Reference (i IntegerType))))))))))))
      (structs ()) (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
  pp_compile source ;
  [%expect
    {|
    (Error
     (((TypeError ((StructType 72) (StructType 73))))
      ((bindings ())
       (structs
        ((73
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 73))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (73 ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 73)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 73))) value IntegerType))
                      (Value (Integer 10)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (StructType 10))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                    (Return
                     (Value
                      (Struct
                       (10
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            (73
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType))))))))))))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 73))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (73 ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 73)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 73))) value IntegerType))
                         (Value (Integer 10))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 10))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 10))))))))
                       (Return
                        (Value
                         (Struct
                          (10
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               (73
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType)))))))))))))))))))))))
             ((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 73))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value (Struct (73 ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 73)))
         (72
          ((struct_fields ((value ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 72))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
             (serialize
              ((function_signature
                ((function_params
                  ((self (StructType 72)) (builder (StructType 3))))
                 (function_returns (StructType 3))))
               (function_impl
                (Fn
                 ((Return
                   (FunctionCall
                    ((ResolvedReference (serialize_int <opaque>))
                     ((Reference (builder (StructType 3)))
                      (StructField
                       ((Reference (self (StructType 72))) value IntegerType))
                      (Value (Integer 99)))))))))))
             (deserialize
              ((function_signature
                ((function_params ((s (StructType 6))))
                 (function_returns (StructType 10))))
               (function_impl
                (Fn
                 ((Block
                   ((Let
                     ((res
                       (FunctionCall
                        ((ResolvedReference (load_int <opaque>))
                         ((Reference (s (StructType 6))) (Value (Integer 99))))))))
                    (Return
                     (Value
                      (Struct
                       (10
                        ((slice
                          (StructField
                           ((Reference (res (StructType 5))) slice
                            (StructType 6))))
                         (value
                          (Value
                           (Struct
                            (72
                             ((value
                               (StructField
                                ((Reference (res (StructType 5))) value
                                 IntegerType))))))))))))))))))))
             (from
              ((function_signature
                ((function_params ((i IntegerType)))
                 (function_returns (StructType 72))))
               (function_impl
                (Fn
                 ((Return
                   (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
           (struct_impls
            (((impl_interface -1)
              (impl_methods
               ((serialize
                 ((function_signature
                   ((function_params
                     ((self (StructType 72)) (builder (StructType 3))))
                    (function_returns (StructType 3))))
                  (function_impl
                   (Fn
                    ((Return
                      (FunctionCall
                       ((ResolvedReference (serialize_int <opaque>))
                        ((Reference (builder (StructType 3)))
                         (StructField
                          ((Reference (self (StructType 72))) value IntegerType))
                         (Value (Integer 99))))))))))))))
             ((impl_interface -3)
              (impl_methods
               ((deserialize
                 ((function_signature
                   ((function_params ((s (StructType 6))))
                    (function_returns (StructType 10))))
                  (function_impl
                   (Fn
                    ((Block
                      ((Let
                        ((res
                          (FunctionCall
                           ((ResolvedReference (load_int <opaque>))
                            ((Reference (s (StructType 6))) (Value (Integer 99))))))))
                       (Return
                        (Value
                         (Struct
                          (10
                           ((slice
                             (StructField
                              ((Reference (res (StructType 5))) slice
                               (StructType 6))))
                            (value
                             (Value
                              (Struct
                               (72
                                ((value
                                  (StructField
                                   ((Reference (res (StructType 5))) value
                                    IntegerType)))))))))))))))))))))))
             ((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((i IntegerType)))
                    (function_returns (StructType 72))))
                  (function_impl
                   (Fn
                    ((Return
                      (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
           (struct_id 72)))))
       (interfaces
        ((74
          ((interface_methods
            ((from
              ((function_params ((from (StructType 73))))
               (function_returns SelfType)))))))))
       (type_counter <opaque>) (memoized_fcalls <opaque>)))) |}]

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
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 73))))))
      (structs
       ((73
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
          (struct_id 73)))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((empty (Value (Struct (74 ())))) (Empty (Value (Type (StructType 74))))
        (Make (Value (Type (InterfaceType 72))))))
      (structs
       ((74
         ((struct_fields ())
          (struct_methods
           ((new
             ((function_signature
               ((function_params ()) (function_returns (StructType 74))))
              (function_impl (Fn ((Return (Value (Struct (74 ())))))))))))
          (struct_impls
           (((impl_interface 72)
             (impl_methods
              ((new
                ((function_signature
                  ((function_params ()) (function_returns (StructType 74))))
                 (function_impl (Fn ((Return (Value (Struct (74 ()))))))))))))))
          (struct_id 74)))))
      (interfaces
       ((72
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((serialize_outer
         (Value
          (Function
           ((function_signature
             ((function_params ((self (StructType 75)) (b (StructType 3))))
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
                            ((self (StructType 44)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Return
                             (FunctionCall
                              ((ResolvedReference (serialize_int <opaque>))
                               ((Reference (builder (StructType 3)))
                                (StructField
                                 ((Reference (self (StructType 44))) value
                                  IntegerType))
                                (Value (Integer 32))))))))))))
                      ((StructField
                        ((Reference (self (StructType 75))) y (StructType 44)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (Outer (Value (Type (StructType 75))))
        (Inner (Value (Type (StructType 73))))))
      (structs
       ((75
         ((struct_fields
           ((y ((field_type (StructType 44))))
            (z ((field_type (StructType 73))))))
          (struct_methods ()) (struct_impls ()) (struct_id 75)))
        (73
         ((struct_fields ((x ((field_type (StructType 44))))))
          (struct_methods ()) (struct_impls ()) (struct_id 73)))))
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
  pp_compile source ;
  [%expect
    {|
    (Error
     (((TypeError ((TypeN 0) (TypeN 1))))
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
    (Ok
     ((bindings
       ((b
         (Value (UnionVariant ((Struct (44 ((value (Value (Integer 1)))))) 73))))
        (a (Value (UnionVariant ((Integer 10) 73))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 73))))
              (function_returns (UnionType 73))))
            (function_impl (Fn ((Return (Reference (value (UnionType 73)))))))))))
        (Uni (Value (Type (UnionType 73))))))
      (structs ())
      (unions
       ((73
         ((cases
           (((StructType 44) (Discriminator 1)) (IntegerType (Discriminator 0))))
          (union_methods ())
          (union_impls
           (((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v IntegerType)))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return (MakeUnionVariant ((Reference (v IntegerType)) 73)))))))))))
            ((impl_interface 74)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v (StructType 44))))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return
                     (MakeUnionVariant ((Reference (v (StructType 44))) 73)))))))))))))
          (union_id 73)))))
      (interfaces
       ((74
         ((interface_methods
           ((from
             ((function_params ((from (StructType 44))))
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
  pp_compile source ;
  [%expect
    {|
    (Error
     (((DuplicateVariant IntegerType))
      ((bindings
        ((b (Value (Type (UnionType 75)))) (a (Value (Type (UnionType 73))))
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
                              (function_returns (UnionType 72))))
                            (function_impl
                             (Fn
                              ((Return
                                (MakeUnionVariant
                                 ((Reference (v IntegerType)) 72)))))))))))))
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
                              (function_returns (UnionType 72))))
                            (function_impl
                             (Fn
                              ((Return
                                (MakeUnionVariant
                                 ((Reference
                                   (v (ExprType (Reference (T (TypeN 0))))))
                                  72)))))))))))))))
                   (mk_union_id 72)))))))))))))
       (structs ())
       (unions
        ((75
          ((cases ((IntegerType (Discriminator 0)))) (union_methods ())
           (union_impls
            (((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v IntegerType)))
                    (function_returns (UnionType 75))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v IntegerType)) 75)))))))))))
             ((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (ExprType (Reference (T (TypeN 0)))))))
                    (function_returns (UnionType 75))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant
                       ((Reference (v (ExprType (Reference (T (TypeN 0)))))) 75)))))))))))))
           (union_id 75)))
         (73
          ((cases
            (((BuiltinType Builder) (Discriminator 1))
             (IntegerType (Discriminator 0))))
           (union_methods ())
           (union_impls
            (((impl_interface 11)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v IntegerType)))
                    (function_returns (UnionType 73))))
                  (function_impl
                   (Fn
                    ((Return (MakeUnionVariant ((Reference (v IntegerType)) 73)))))))))))
             ((impl_interface 74)
              (impl_methods
               ((from
                 ((function_signature
                   ((function_params ((v (ExprType (Reference (T (TypeN 0)))))))
                    (function_returns (UnionType 73))))
                  (function_impl
                   (Fn
                    ((Return
                      (MakeUnionVariant
                       ((Reference (v (ExprType (Reference (T (TypeN 0)))))) 73)))))))))))))
           (union_id 73)))))
       (interfaces
        ((74
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
    (Ok
     ((bindings ((Test (Value (Type (UnionType 74))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (unions
       ((74
         ((cases
           (((StructType 43) (Discriminator 1))
            ((StructType 72) (Discriminator 0))))
          (union_methods
           ((id
             ((function_signature
               ((function_params ((self (UnionType 74))))
                (function_returns (UnionType 74))))
              (function_impl (Fn ((Return (Reference (self (UnionType 74)))))))))))
          (union_impls
           (((impl_interface 75)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v (StructType 72))))
                   (function_returns (UnionType 74))))
                 (function_impl
                  (Fn
                   ((Return
                     (MakeUnionVariant ((Reference (v (StructType 72))) 74)))))))))))
            ((impl_interface 76)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v (StructType 43))))
                   (function_returns (UnionType 74))))
                 (function_impl
                  (Fn
                   ((Return
                     (MakeUnionVariant ((Reference (v (StructType 43))) 74)))))))))))))
          (union_id 74)))))
      (interfaces
       ((76
         ((interface_methods
           ((from
             ((function_params ((from (StructType 43))))
              (function_returns SelfType)))))))
        (75
         ((interface_methods
           ((from
             ((function_params ((from (StructType 72))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((y (Value (Struct (75 ())))) (foo_empty (Value (Struct (76 ()))))
        (Empty (Value (Type (StructType 75)))) (x (Value (Integer 10)))
        (foo (Value (Struct (73 ()))))
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
                          ((self (StructType 72))
                           (x (ExprType (Reference (X (TypeN 0)))))))
                         (function_returns (ExprType (Reference (X (TypeN 0)))))))
                       (function_impl
                        (Fn
                         ((Return
                           (Reference (x (ExprType (Reference (X (TypeN 0)))))))))))))))
                  (mk_impls ()) (mk_struct_id 72)))))))))))))
      (structs
       ((76
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 76)) (x (StructType 75))))
                (function_returns (StructType 75))))
              (function_impl (Fn ((Return (Reference (x (StructType 75)))))))))))
          (struct_impls ()) (struct_id 76)))
        (75
         ((struct_fields ()) (struct_methods ()) (struct_impls ())
          (struct_id 75)))
        (73
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 73)) (x IntegerType)))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Reference (x IntegerType))))))))))
          (struct_impls ()) (struct_id 73)))))
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
             ((function_params ((i (UnionType 73))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Break
                (Switch
                 ((switch_condition (Reference (i (UnionType 73))))
                  (branches
                   (((branch_ty (StructType 44)) (branch_var vax)
                     (branch_stmt (Block ((Return (Value (Integer 32)))))))
                    ((branch_ty (StructType 43)) (branch_var vax)
                     (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))))
        (Ints (Value (Type (UnionType 73))))))
      (structs ())
      (unions
       ((73
         ((cases
           (((StructType 43) (Discriminator 1))
            ((StructType 44) (Discriminator 0))))
          (union_methods ())
          (union_impls
           (((impl_interface 74)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v (StructType 44))))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return
                     (MakeUnionVariant ((Reference (v (StructType 44))) 73)))))))))))
            ((impl_interface 75)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((v (StructType 43))))
                   (function_returns (UnionType 73))))
                 (function_impl
                  (Fn
                   ((Return
                     (MakeUnionVariant ((Reference (v (StructType 43))) 73)))))))))))))
          (union_id 73)))))
      (interfaces
       ((75
         ((interface_methods
           ((from
             ((function_params ((from (StructType 43))))
              (function_returns SelfType)))))))
        (74
         ((interface_methods
           ((from
             ((function_params ((from (StructType 44))))
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
  pp_compile source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Struct (44 ((value (Value (Integer 2))))))))
        (a (Value (Struct (72 ((value (Value (Integer 1))))))))))
      (structs
       ((72
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 72)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Return
                  (FunctionCall
                   ((ResolvedReference (serialize_int <opaque>))
                    ((Reference (builder (StructType 3)))
                     (StructField
                      ((Reference (self (StructType 72))) value IntegerType))
                     (Value (Integer 257)))))))))))
            (deserialize
             ((function_signature
               ((function_params ((s (StructType 6))))
                (function_returns (StructType 10))))
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
                      (10
                       ((slice
                         (StructField
                          ((Reference (res (StructType 5))) slice (StructType 6))))
                        (value
                         (Value
                          (Struct
                           (72
                            ((value
                              (StructField
                               ((Reference (res (StructType 5))) value
                                IntegerType))))))))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 72))))
              (function_impl
               (Fn
                ((Return
                  (Value (Struct (72 ((value (Reference (i IntegerType)))))))))))))))
          (struct_impls
           (((impl_interface -1)
             (impl_methods
              ((serialize
                ((function_signature
                  ((function_params
                    ((self (StructType 72)) (builder (StructType 3))))
                   (function_returns (StructType 3))))
                 (function_impl
                  (Fn
                   ((Return
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 72))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            ((impl_interface -3)
             (impl_methods
              ((deserialize
                ((function_signature
                  ((function_params ((s (StructType 6))))
                   (function_returns (StructType 10))))
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
                         (10
                          ((slice
                            (StructField
                             ((Reference (res (StructType 5))) slice
                              (StructType 6))))
                           (value
                            (Value
                             (Struct
                              (72
                               ((value
                                 (StructField
                                  ((Reference (res (StructType 5))) value
                                   IntegerType)))))))))))))))))))))))
            ((impl_interface 11)
             (impl_methods
              ((from
                ((function_signature
                  ((function_params ((i IntegerType)))
                   (function_returns (StructType 72))))
                 (function_impl
                  (Fn
                   ((Return
                     (Value (Struct (72 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_id 72)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "let binding with a non-matching type" =
  let source = {|
      let a: Bool = 1;
    |} in
  pp_compile source ;
  [%expect
    {|
    (Error
     (((TypeError (BoolType IntegerType)) (TypeError (BoolType IntegerType)))
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
    (Ok
     ((bindings
       ((must_be_one (Value (Integer 1)))
        (concrete_beeper
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 74))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Return
                (FunctionCall
                 ((Value
                   (Function
                    ((function_signature
                      ((function_params ((self (StructType 74))))
                       (function_returns IntegerType)))
                     (function_impl (Fn ((Return (Value (Integer 1)))))))))
                  ((Reference (t (StructType 74))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 72))))
              (function_returns
               (FunctionType
                ((function_params ((t (Dependent T (InterfaceType 72)))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              ((Return
                (MkFunction
                 ((function_signature
                   ((function_params
                     ((t (ExprType (Reference (T (InterfaceType 72)))))))
                    (function_returns IntegerType)))
                  (function_impl
                   (Fn
                    ((Return
                      (IntfMethodCall
                       ((intf_instance (Reference (T (InterfaceType 72))))
                        (intf_def 72)
                        (intf_method
                         (beep
                          ((function_params ((self SelfType)))
                           (function_returns IntegerType))))
                        (intf_args
                         ((Reference
                           (t (ExprType (Reference (T (InterfaceType 72))))))))))))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 74))))
        (Beep (Value (Type (InterfaceType 72))))))
      (structs
       ((74
         ((struct_fields ())
          (struct_methods
           ((beep
             ((function_signature
               ((function_params ((self (StructType 74))))
                (function_returns IntegerType)))
              (function_impl (Fn ((Return (Value (Integer 1))))))))))
          (struct_impls
           (((impl_interface 72)
             (impl_methods
              ((beep
                ((function_signature
                  ((function_params ((self (StructType 74))))
                   (function_returns IntegerType)))
                 (function_impl (Fn ((Return (Value (Integer 1)))))))))))))
          (struct_id 74)))))
      (interfaces
       ((72
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
             ((function_params ((t (StructType 73))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((DestructuringLet
                  ((destructuring_let ((x x) (y y2) (z z)))
                   (destructuring_let_expr (Reference (t (StructType 73))))
                   (destructuring_let_rest false)))
                 (Return (Reference (y2 HoleType))))))))))))
        (T (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 73)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
     (((MissingField (73 x)) (MissingField (73 z)))
      ((bindings
        ((test
          (Value
           (Function
            ((function_signature
              ((function_params ((t (StructType 73))))
               (function_returns IntegerType)))
             (function_impl
              (Fn
               ((Block
                 ((DestructuringLet
                   ((destructuring_let ((y y2)))
                    (destructuring_let_expr (Reference (t (StructType 73))))
                    (destructuring_let_rest false)))
                  (Return (Reference (y2 HoleType))))))))))))
         (T (Value (Type (StructType 73))))))
       (structs
        ((73
          ((struct_fields
            ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
             (z ((field_type IntegerType)))))
           (struct_methods ()) (struct_impls ()) (struct_id 73)))))
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
    (Ok
     ((bindings
       ((test
         (Value
          (Function
           ((function_signature
             ((function_params ((t (StructType 73))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((DestructuringLet
                  ((destructuring_let ((y y2)))
                   (destructuring_let_expr (Reference (t (StructType 73))))
                   (destructuring_let_rest true)))
                 (Return (Reference (y2 HoleType))))))))))))
        (T (Value (Type (StructType 73))))))
      (structs
       ((73
         ((struct_fields
           ((x ((field_type IntegerType))) (y ((field_type IntegerType)))
            (z ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 73)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

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
     (((TypeError ((InterfaceType 72) (TypeN 0))))
      ((bindings
        ((a (Value Void)) (Foo (Value (Type (StructType 74))))
         (ExpectedIntf
          (Value
           (Function
            ((function_signature
              ((function_params ((T (InterfaceType 72))))
               (function_returns HoleType)))
             (function_impl (Fn ((Block ()))))))))
         (Intf (Value (Type (InterfaceType 72))))))
       (structs
        ((74
          ((struct_fields ()) (struct_methods ()) (struct_impls ())
           (struct_id 74)))))
       (interfaces ((72 ((interface_methods ()))))) (type_counter <opaque>)
       (memoized_fcalls <opaque>)))) |}]
