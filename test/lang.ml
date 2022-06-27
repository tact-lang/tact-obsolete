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
     ((bindings ((T (Value (Type (StructType 42))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "binding resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 42))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
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
       ((B (Value (Type (StructType 42)))) (A (Value (Type (StructType 42))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings ((T (Value (Type (StructType 44))))))
      (structs
       ((44
         ((struct_fields ((t ((field_type (StructType 42))))))
          (struct_methods ()) (struct_impls ()) (struct_id 44)))
        (42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
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
       ((a (Value (Struct (42 ((value (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 42))))
              (function_returns (StructType 42))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (i (StructType 42))))))))))))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
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
                 (Break (Expr (ResolvedReference (v <opaque>)))))))))))))))
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
       ((bindings ((MyType (Value (Type (StructType 44))))))
        (structs
         ((44
           ((struct_fields
             ((a ((field_type (StructType 42)))) (b ((field_type BoolType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 44)))
          (42
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 42)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 42))) value IntegerType))
                          (Value (Integer 257))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 42))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Value
                             (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
            (struct_id 42)))))
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
           ((a (Value (Type (StructType 42)))) (a (Value (Type BoolType)))))
          (mk_methods ()) (mk_impls ()) (mk_struct_id -1))))
       ((bindings ((MyType (Value (Type (StructType 44))))))
        (structs
         ((44
           ((struct_fields
             ((a ((field_type (StructType 42)))) (a ((field_type BoolType)))))
            (struct_methods ()) (struct_impls ()) (struct_id 44)))
          (42
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 42)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 42))) value IntegerType))
                          (Value (Integer 257))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 42))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Value
                             (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
            (struct_id 42)))))
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
       ((TA (Value (Type (StructType 44))))
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
                  (mk_methods ()) (mk_impls ()) (mk_struct_id 42)))))))))))))
      (structs
       ((44
         ((struct_fields ((a ((field_type (StructType 43))))))
          (struct_methods ()) (struct_impls ()) (struct_id 44)))
        (43
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 43))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (43 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 43)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 43))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 43))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (43 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 43))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (43 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 43)))))
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
              (function_impl (Fn ((Block ((Break (Expr (Value (Integer 1)))))))))))))))
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
       ((b (Value (Struct (42 ((value (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 42))))
              (function_returns (StructType 42))))
            (function_impl
             (Fn
              ((Block
                ((Let ((a (Reference (i (StructType 42))))))
                 (Break (Expr (Reference (a (StructType 42))))))))))))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
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
             ((function_params ((x (StructType 42))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (x (StructType 42)))
                       (Reference (x (StructType 42)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference (a (StructType 42)))
                       (Reference (a (StructType 42))))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params ((i (StructType 42)) (i_ (StructType 42))))
              (function_returns (StructType 42))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (i (StructType 42))))))))))))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
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
            (function_impl
             (Fn ((Block ((Break (Expr (ResolvedReference (i <opaque>)))))))))))))
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
       ((res (Value (Integer 1))) (foo (Value (Struct (43 ()))))
        (Foo (Value (Type (StructType 43))))))
      (structs
       ((43
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 43)) (i IntegerType)))
                (function_returns IntegerType)))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
          (struct_impls ()) (struct_id 43)))))
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
     ((bindings ((res (Value (Integer 1))) (Foo (Value (Type (StructType 43))))))
      (structs
       ((43
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns IntegerType)))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
          (struct_impls ()) (struct_id 43)))))
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
     ((bindings ((Foo (Value (Type (StructType 43))))))
      (structs
       ((43
         ((struct_fields ())
          (struct_methods
           ((bar
             ((function_signature
               ((function_params ((self (StructType 43))))
                (function_returns (StructType 43))))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (self (StructType 43))))))))))))))
          (struct_impls ()) (struct_id 43)))))
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
         ((res (Value (Integer 1))) (foo (Value (UnionVariant ((Bool true) 43))))
          (make_foo
           (Value
            (Function
             ((function_signature
               ((function_params ((foo (UnionType 43))))
                (function_returns (UnionType 43))))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (foo (UnionType 43))))))))))))))
          (Foo (Value (Type (UnionType 43))))))
        (structs ())
        (unions
         ((43
           ((cases ((BoolType (Discriminator 0))))
            (union_methods
             ((bar
               ((function_signature
                 ((function_params ((self (UnionType 43)) (i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl
                 (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 44))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v (ExprType (Value (Type BoolType))))))
                       (function_returns (UnionType 43))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Value (Type BoolType))))) 43)))))))))))))))
            (union_id 43)))))
        (interfaces
         ((44
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
               ((function_params ((foo (UnionType 43))))
                (function_returns (UnionType 43))))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (foo (UnionType 43))))))))))))))
          (Foo (Value (Type (UnionType 43))))))
        (structs ())
        (unions
         ((43
           ((cases ((BoolType (Discriminator 0))))
            (union_methods
             ((bar
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns IntegerType)))
                (function_impl
                 (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 44))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v (ExprType (Value (Type BoolType))))))
                       (function_returns (UnionType 43))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Value (Type BoolType))))) 43)))))))))))))))
            (union_id 43)))))
        (interfaces
         ((44
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
         (Value (Struct (43 ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T (Value (Type (StructType 43))))))
      (structs
       ((43
         ((struct_fields
           ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 43)))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError ((StructType 35) (StructType 36)))
       ((bindings
         ((foo
           (Value
            (Function
             ((function_signature
               ((function_params ((i (StructType 36))))
                (function_returns (StructType 35))))
              (function_impl
               (Fn ((Block ((Return (Reference (i (StructType 36)))))))))))))))
        (structs ())
        (interfaces
         ((42
           ((interface_methods
             ((from
               ((function_params ((from (StructType 36))))
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
            (function_impl (Fn ((Block ((Return (Reference (i IntegerType))))))))))))))
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
              ((Block
                ((Break
                  (If
                   ((if_condition (Value (Bool true)))
                    (if_then (Block ((Return (Value (Integer 1))))))
                    (if_else ((Block ((Return (Value (Integer 2)))))))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ()) (function_returns BoolType)))
            (function_impl (Fn ((Block ((Break (Expr (Value (Bool true)))))))))))))))
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
     (((TypeError ((StructType 42) (StructType 43)))
       ((bindings ())
        (structs
         ((43
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 43))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (43 ((value (Reference (i IntegerType))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 43)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 43))) value IntegerType))
                          (Value (Integer 10))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 43))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (43 ((value (Reference (i IntegerType))))))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 43))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Value
                             (Struct (43 ((value (Reference (i IntegerType)))))))))))))))))))))))
            (struct_id 43)))
          (42
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 42)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 42))) value IntegerType))
                          (Value (Integer 99))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value
                        (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 42))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Value
                             (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
            (struct_id 42)))))
        (interfaces
         ((44
           ((interface_methods
             ((from
               ((function_params ((from (StructType 43))))
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
       ((one (Value (Integer 1))) (Left (Value (Type (StructType 43))))))
      (structs
       ((43
         ((struct_fields ())
          (struct_methods
           ((op
             ((function_signature
               ((function_params ((left IntegerType) (right IntegerType)))
                (function_returns IntegerType)))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (left IntegerType)))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType -10))))
             (impl_methods
              ((op
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((left IntegerType) (right IntegerType)))
                     (function_returns IntegerType)))
                   (function_impl
                    (Fn
                     ((Block ((Break (Expr (Reference (left IntegerType))))))))))))))))))
          (struct_id 43)))))
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
       ((empty (Value (Struct (44 ())))) (Empty (Value (Type (StructType 44))))
        (Make (Value (Type (InterfaceType 42))))))
      (structs
       ((44
         ((struct_fields ())
          (struct_methods
           ((new
             ((function_signature
               ((function_params ()) (function_returns (StructType 44))))
              (function_impl
               (Fn ((Block ((Break (Expr (Value (Struct (44 ()))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 42))))
             (impl_methods
              ((new
                (Value
                 (Function
                  ((function_signature
                    ((function_params ()) (function_returns (StructType 44))))
                   (function_impl
                    (Fn ((Block ((Break (Expr (Value (Struct (44 ())))))))))))))))))))
          (struct_id 44)))))
      (interfaces
       ((42
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
             ((function_params ((self (StructType 45)) (b (StructType 3))))
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
                            ((self (StructType 36)) (builder (StructType 3))))
                           (function_returns (StructType 3))))
                         (function_impl
                          (Fn
                           ((Block
                             ((Break
                               (Expr
                                (FunctionCall
                                 ((ResolvedReference (serialize_int <opaque>))
                                  ((Reference (builder (StructType 3)))
                                   (StructField
                                    ((Reference (self (StructType 36))) value
                                     IntegerType))
                                   (Value (Integer 32)))))))))))))))
                      ((StructField
                        ((Reference (self (StructType 45))) y (StructType 36)))
                       (Reference (b (StructType 3)))))))))
                 (Return (Reference (b (StructType 3)))))))))))))
        (Outer (Value (Type (StructType 45))))
        (Inner (Value (Type (StructType 43))))))
      (structs
       ((45
         ((struct_fields
           ((y ((field_type (StructType 36))))
            (z ((field_type (StructType 43))))))
          (struct_methods ()) (struct_impls ()) (struct_id 45)))
        (43
         ((struct_fields ((x ((field_type (StructType 36))))))
          (struct_methods ()) (struct_impls ()) (struct_id 43)))))
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
              ((Block
                ((Break
                  (Expr
                   (MkFunction
                    ((function_signature
                      ((function_params
                        ((x (ExprType (Reference (X (TypeN 0)))))))
                       (function_returns (ExprType (Reference (X (TypeN 0)))))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))))))))))))))))
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
              ((Block
                ((Break
                  (Expr
                   (FunctionCall
                    ((ResolvedReference (identity <opaque>))
                     ((Reference (Y (TypeN 0)))))))))))))))))
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
                        ((Block
                          ((Break
                            (Expr
                             (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))))))
                 (Break
                  (Expr
                   (Reference
                    (f
                     (FunctionType
                      ((function_params
                        ((x (ExprType (Reference (X (TypeN 0)))))))
                       (function_returns (ExprType (Reference (X (TypeN 0))))))))))))))))))))))
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
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (X (TypeN 0))))))))))))))))
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
         (Value (UnionVariant ((Struct (36 ((value (Value (Integer 1)))))) 43))))
        (a (Value (UnionVariant ((Integer 10) 43))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((value (UnionType 43))))
              (function_returns (UnionType 43))))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (value (UnionType 43))))))))))))))
        (Uni (Value (Type (UnionType 43))))))
      (structs ())
      (unions
       ((43
         ((cases
           (((StructType 36) (Discriminator 0)) (IntegerType (Discriminator 1))))
          (union_methods ())
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type IntegerType))))))
                     (function_returns (UnionType 43))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference (v (ExprType (Value (Type IntegerType)))))
                         43)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 44))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 36)))))))
                     (function_returns (UnionType 43))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 36))))))
                         43)))))))))))))))
          (union_id 43)))))
      (interfaces
       ((44
         ((interface_methods
           ((from
             ((function_params ((from (StructType 36))))
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
         ((b (Value (Type (UnionType 45)))) (a (Value (Type (UnionType 43))))
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
                               (function_returns (UnionType 42))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Value
                                    (Type (ExprType (Reference (v IntegerType)))))
                                   42)))))))))))))
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
                               (function_returns (UnionType 42))))
                             (function_impl
                              (Fn
                               ((Return
                                 (MakeUnionVariant
                                  ((Value
                                    (Type
                                     (ExprType
                                      (Reference
                                       (v (ExprType (Reference (T (TypeN 0)))))))))
                                   42)))))))))))))))
                    (mk_union_id 42)))))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
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
                                  (function_returns (UnionType 42))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (MakeUnionVariant
                                     ((Reference (v IntegerType)) 42)))))))))))))
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
                                  (function_returns (UnionType 42))))
                                (function_impl
                                 (Fn
                                  ((Return
                                    (MakeUnionVariant
                                     ((Reference
                                       (v (ExprType (Reference (T (TypeN 0))))))
                                      42)))))))))))))))
                       (mk_union_id 42))))))))))))))))
        (structs ())
        (unions
         ((45
           ((cases ((IntegerType (Discriminator 0)))) (union_methods ())
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v IntegerType)))
                       (function_returns (UnionType 45))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant ((Reference (v IntegerType)) 45)))))))))))))
              ((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((v (ExprType (Reference (T (TypeN 0)))))))
                       (function_returns (UnionType 45))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Reference (T (TypeN 0))))))
                           45)))))))))))))))
            (union_id 45)))
          (43
           ((cases
             (((BuiltinType Builder) (Discriminator 0))
              (IntegerType (Discriminator 1))))
            (union_methods ())
            (union_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((v IntegerType)))
                       (function_returns (UnionType 43))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant ((Reference (v IntegerType)) 43)))))))))))))
              ((impl_interface (Value (Type (InterfaceType 44))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params
                        ((v (ExprType (Reference (T (TypeN 0)))))))
                       (function_returns (UnionType 43))))
                     (function_impl
                      (Fn
                       ((Return
                         (MakeUnionVariant
                          ((Reference (v (ExprType (Reference (T (TypeN 0))))))
                           43)))))))))))))))
            (union_id 43)))))
        (interfaces
         ((44
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
     ((bindings ((Test (Value (Type (UnionType 44))))))
      (structs
       ((42
         ((struct_fields ((value ((field_type IntegerType)))))
          (struct_methods
           ((new
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
            (serialize
             ((function_signature
               ((function_params
                 ((self (StructType 42)) (builder (StructType 3))))
                (function_returns (StructType 3))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (FunctionCall
                      ((ResolvedReference (serialize_int <opaque>))
                       ((Reference (builder (StructType 3)))
                        (StructField
                         ((Reference (self (StructType 42))) value IntegerType))
                        (Value (Integer 257))))))))))))))
            (from
             ((function_signature
               ((function_params ((i IntegerType)))
                (function_returns (StructType 42))))
              (function_impl
               (Fn
                ((Block
                  ((Break
                    (Expr
                     (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 9))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((i IntegerType)))
                     (function_returns (StructType 42))))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
          (struct_id 42)))))
      (unions
       ((44
         ((cases
           (((StructType 35) (Discriminator 0))
            ((StructType 42) (Discriminator 1))))
          (union_methods
           ((id
             ((function_signature
               ((function_params ((self (UnionType 44))))
                (function_returns (UnionType 44))))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (self (UnionType 44))))))))))))))
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 45))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 42)))))))
                     (function_returns (UnionType 44))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 42))))))
                         44)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 46))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 35)))))))
                     (function_returns (UnionType 44))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 35))))))
                         44)))))))))))))))
          (union_id 44)))))
      (interfaces
       ((46
         ((interface_methods
           ((from
             ((function_params ((from (StructType 35))))
              (function_returns SelfType)))))))
        (45
         ((interface_methods
           ((from
             ((function_params ((from (StructType 42))))
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
       ((y (Value (Struct (45 ())))) (foo_empty (Value (Struct (46 ()))))
        (Empty (Value (Type (StructType 45)))) (x (Value (Integer 10)))
        (foo (Value (Struct (43 ()))))
        (Foo
         (Value
          (Function
           ((function_signature
             ((function_params ((X (TypeN 0)))) (function_returns (TypeN 0))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (MkStructDef
                    ((mk_struct_fields ())
                     (mk_methods
                      ((id
                        (MkFunction
                         ((function_signature
                           ((function_params
                             ((self (StructType 42))
                              (x (ExprType (Reference (X (TypeN 0)))))))
                            (function_returns
                             (ExprType (Reference (X (TypeN 0)))))))
                          (function_impl
                           (Fn
                            ((Block
                              ((Break
                                (Expr
                                 (Reference
                                  (x (ExprType (Reference (X (TypeN 0))))))))))))))))))
                     (mk_impls ()) (mk_struct_id 42))))))))))))))))
      (structs
       ((46
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 46)) (x (StructType 45))))
                (function_returns (StructType 45))))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (x (StructType 45))))))))))))))
          (struct_impls ()) (struct_id 46)))
        (45
         ((struct_fields ()) (struct_methods ()) (struct_impls ())
          (struct_id 45)))
        (43
         ((struct_fields ())
          (struct_methods
           ((id
             ((function_signature
               ((function_params ((self (StructType 43)) (x IntegerType)))
                (function_returns IntegerType)))
              (function_impl
               (Fn ((Block ((Break (Expr (Reference (x IntegerType)))))))))))))
          (struct_impls ()) (struct_id 43)))))
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
             ((function_params ((i (UnionType 43))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Switch
                   ((switch_condition (Reference (i (UnionType 43))))
                    (branches
                     (((branch_ty (StructType 36)) (branch_var vax)
                       (branch_stmt (Block ((Return (Value (Integer 32)))))))
                      ((branch_ty (StructType 35)) (branch_var vax)
                       (branch_stmt (Block ((Return (Value (Integer 64)))))))))))))))))))))
        (Ints (Value (Type (UnionType 43))))))
      (structs ())
      (unions
       ((43
         ((cases
           (((StructType 35) (Discriminator 0))
            ((StructType 36) (Discriminator 1))))
          (union_methods ())
          (union_impls
           (((impl_interface (Value (Type (InterfaceType 44))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 36)))))))
                     (function_returns (UnionType 43))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 36))))))
                         43)))))))))))))
            ((impl_interface (Value (Type (InterfaceType 45))))
             (impl_methods
              ((from
                (Value
                 (Function
                  ((function_signature
                    ((function_params
                      ((v (ExprType (Value (Type (StructType 35)))))))
                     (function_returns (UnionType 43))))
                   (function_impl
                    (Fn
                     ((Return
                       (MakeUnionVariant
                        ((Reference
                          (v (ExprType (Value (Type (StructType 35))))))
                         43)))))))))))))))
          (union_id 43)))))
      (interfaces
       ((45
         ((interface_methods
           ((from
             ((function_params ((from (StructType 35))))
              (function_returns SelfType)))))))
        (44
         ((interface_methods
           ((from
             ((function_params ((from (StructType 36))))
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
              ((Block
                ((Break
                  (Expr
                   (FunctionCall
                    ((ResolvedReference (left <opaque>))
                     ((Value (Integer 10)) (Reference (y IntegerType))))))))))))))))
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
              ((Block
                ((Break
                  (Expr
                   (MkFunction
                    ((function_signature
                      ((function_params ((y IntegerType)))
                       (function_returns IntegerType)))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (FunctionCall
                             ((ResolvedReference (left <opaque>))
                              ((Reference (x IntegerType))
                               (Reference (y IntegerType)))))))))))))))))))))))))
        (left
         (Value
          (Function
           ((function_signature
             ((function_params ((x IntegerType) (y IntegerType)))
              (function_returns IntegerType)))
            (function_impl
             (Fn ((Block ((Break (Expr (Reference (x IntegerType)))))))))))))))
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
         ((a (Value (Struct (36 ((value (Value (Integer 2))))))))
          (a (Value (Struct (42 ((value (Value (Integer 1))))))))))
        (structs
         ((42
           ((struct_fields ((value ((field_type IntegerType)))))
            (struct_methods
             ((new
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))
              (serialize
               ((function_signature
                 ((function_params
                   ((self (StructType 42)) (builder (StructType 3))))
                  (function_returns (StructType 3))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (FunctionCall
                        ((ResolvedReference (serialize_int <opaque>))
                         ((Reference (builder (StructType 3)))
                          (StructField
                           ((Reference (self (StructType 42))) value IntegerType))
                          (Value (Integer 257))))))))))))))
              (from
               ((function_signature
                 ((function_params ((i IntegerType)))
                  (function_returns (StructType 42))))
                (function_impl
                 (Fn
                  ((Block
                    ((Break
                      (Expr
                       (Value (Struct (42 ((value (Reference (i IntegerType))))))))))))))))))
            (struct_impls
             (((impl_interface (Value (Type (InterfaceType 9))))
               (impl_methods
                ((from
                  (Value
                   (Function
                    ((function_signature
                      ((function_params ((i IntegerType)))
                       (function_returns (StructType 42))))
                     (function_impl
                      (Fn
                       ((Block
                         ((Break
                           (Expr
                            (Value
                             (Struct (42 ((value (Reference (i IntegerType)))))))))))))))))))))))
            (struct_id 42)))))
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
             ((function_params ((t (StructType 44))))
              (function_returns IntegerType)))
            (function_impl
             (Fn
              ((Block
                ((Return
                  (FunctionCall
                   ((Value
                     (Function
                      ((function_signature
                        ((function_params ((self (StructType 44))))
                         (function_returns IntegerType)))
                       (function_impl
                        (Fn ((Block ((Break (Expr (Value (Integer 1))))))))))))
                    ((Reference (t (StructType 44))))))))))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params ((T (InterfaceType 42))))
              (function_returns
               (FunctionType
                ((function_params ((t (Dependent T (InterfaceType 42)))))
                 (function_returns IntegerType))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (MkFunction
                    ((function_signature
                      ((function_params
                        ((t (ExprType (Reference (T (InterfaceType 42)))))))
                       (function_returns IntegerType)))
                     (function_impl
                      (Fn
                       ((Block
                         ((Return
                           (IntfMethodCall
                            ((intf_instance (Reference (T (InterfaceType 42))))
                             (intf_def 42)
                             (intf_method
                              (beep
                               ((function_params ((self SelfType)))
                                (function_returns IntegerType))))
                             (intf_args
                              ((Reference
                                (t (ExprType (Reference (T (InterfaceType 42)))))))))))))))))))))))))))))
        (BeeperImpl1 (Value (Type (StructType 44))))
        (Beep (Value (Type (InterfaceType 42))))))
      (structs
       ((44
         ((struct_fields ())
          (struct_methods
           ((beep
             ((function_signature
               ((function_params ((self (StructType 44))))
                (function_returns IntegerType)))
              (function_impl (Fn ((Block ((Break (Expr (Value (Integer 1)))))))))))))
          (struct_impls
           (((impl_interface (Value (Type (InterfaceType 42))))
             (impl_methods
              ((beep
                (Value
                 (Function
                  ((function_signature
                    ((function_params ((self (StructType 44))))
                     (function_returns IntegerType)))
                   (function_impl
                    (Fn ((Block ((Break (Expr (Value (Integer 1))))))))))))))))))
          (struct_id 44)))))
      (interfaces
       ((42
         ((interface_methods
           ((beep
             ((function_params ((self SelfType))) (function_returns IntegerType)))))))))
      (type_counter <opaque>) (memoized_fcalls <opaque>))) |}]
