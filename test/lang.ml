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

let%expect_test "scope resolution" =
  let source = {|
    let T = Int(257);
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((T
         (Value
          (Type
           (StructType
            ((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0)))))))))) |}]

let%expect_test "binding resolution" =
  let source =
    {|
    let T = Int(257);
    let T_ = T;
    let a = 1;
    let a_ = a;
  |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a_ (Value (Integer 1))) (a (Value (Integer 1)))
        (T_
         (Value
          (Type
           (StructType
            ((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0))))))
        (T
         (Value
          (Type
           (StructType
            ((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0)))))))))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((UnresolvedIdentifier Int256)
       ((bindings
         ((Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

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
       ((B
         (Value
          (Type
           (StructType
            ((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0))))))
        (A
         (Value
          (Type
           (StructType
            ((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0)))))))))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((T
         (Value
          (Type
           (StructType
            ((struct_fields
              ((t
                ((field_type
                  (StructType
                   ((struct_fields ((integer ((field_type IntegerType)))))
                    (struct_methods
                     ((new
                       ((function_signature
                         ((function_params ((integer IntegerType)))
                          (function_returns SelfType)))
                        (function_impl (BuiltinFn (<fun> 4)))))
                      (serialize
                       ((function_signature
                         ((function_params
                           ((self SelfType) (b (BuiltinType Builder))))
                          (function_returns (BuiltinType Builder))))
                        (function_impl
                         (Fn
                          ((Return
                            (Primitive
                             (StoreInt
                              (builder (Reference (b (BuiltinType Builder))))
                              (length (Value (Integer 257)))
                              (integer
                               (StructField
                                ((Reference (self SelfType)) integer)))
                              (signed true)))))))))))
                    (struct_impls ()) (struct_id 0))))))))
             (struct_methods ()) (struct_impls ()) (struct_id 1)))))))))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
  [%expect {|
    (Ok ((bindings ((v (Value (Integer 4))))))) |}]

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
       ((a
         (Value
          (Struct
           (((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0))
            ((integer (Value (Integer 1))))))))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 4)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 257)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 0))))))
              (function_returns
               (StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 4)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 257)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 0))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (i
                     (StructType
                      ((struct_fields ((integer ((field_type IntegerType)))))
                       (struct_methods
                        ((new
                          ((function_signature
                            ((function_params ((integer IntegerType)))
                             (function_returns SelfType)))
                           (function_impl (BuiltinFn (<fun> 4)))))
                         (serialize
                          ((function_signature
                            ((function_params
                              ((self SelfType) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 257)))
                                 (integer
                                  (StructField
                                   ((Reference (self SelfType)) integer)))
                                 (signed true)))))))))))
                       (struct_impls ()) (struct_id 0)))))))))))))))))))) |}]

let%expect_test "compile-time function evaluation within a function" =
  let source =
    {|
    fn test() {
      let v = incr(incr(incr(1)));
      v
    }
  |}
  in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
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
                 (Break (Expr (ResolvedReference (v <opaque>))))))))))))))))) |}]

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
       ((bindings
         ((MyType
           (Value
            (Type
             (StructType
              ((struct_fields
                ((a
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 4)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
                            (function_returns (BuiltinType Builder))))
                          (function_impl
                           (Fn
                            ((Return
                              (Primitive
                               (StoreInt
                                (builder (Reference (b (BuiltinType Builder))))
                                (length (Value (Integer 257)))
                                (integer
                                 (StructField
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 0))))))
                 (b ((field_type BoolType)))))
               (struct_methods ()) (struct_impls ()) (struct_id 2)))))))))) |}]

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
         ((struct_fields
           ((a
             ((field_type
               (StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 4)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 257)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 0))))))
            (a ((field_type BoolType)))))
          (struct_methods ()) (struct_impls ()) (struct_id 3))))
       ((bindings
         ((MyType
           (Value
            (Type
             (StructType
              ((struct_fields
                ((a
                  ((field_type
                    (StructType
                     ((struct_fields ((integer ((field_type IntegerType)))))
                      (struct_methods
                       ((new
                         ((function_signature
                           ((function_params ((integer IntegerType)))
                            (function_returns SelfType)))
                          (function_impl (BuiltinFn (<fun> 4)))))
                        (serialize
                         ((function_signature
                           ((function_params
                             ((self SelfType) (b (BuiltinType Builder))))
                            (function_returns (BuiltinType Builder))))
                          (function_impl
                           (Fn
                            ((Return
                              (Primitive
                               (StoreInt
                                (builder (Reference (b (BuiltinType Builder))))
                                (length (Value (Integer 257)))
                                (integer
                                 (StructField
                                  ((Reference (self SelfType)) integer)))
                                (signed true)))))))))))
                      (struct_impls ()) (struct_id 0))))))
                 (a ((field_type BoolType)))))
               (struct_methods ()) (struct_impls ()) (struct_id 3))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

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
       ((TA
         (Value
          (Type
           (StructType
            ((struct_fields
              ((a
                ((field_type
                  (StructType
                   ((struct_fields ((integer ((field_type IntegerType)))))
                    (struct_methods
                     ((new
                       ((function_signature
                         ((function_params ((integer IntegerType)))
                          (function_returns SelfType)))
                        (function_impl (BuiltinFn (<fun> 4)))))
                      (serialize
                       ((function_signature
                         ((function_params
                           ((self SelfType) (b (BuiltinType Builder))))
                          (function_returns (BuiltinType Builder))))
                        (function_impl
                         (Fn
                          ((Return
                            (Primitive
                             (StoreInt
                              (builder (Reference (b (BuiltinType Builder))))
                              (length (Value (Integer 257)))
                              (integer
                               (StructField
                                ((Reference (self SelfType)) integer)))
                              (signed true)))))))))))
                    (struct_impls ()) (struct_id 0))))))))
             (struct_methods ()) (struct_impls ()) (struct_id 4))))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ((A (TypeN 0)))) (function_returns (TypeN 0))))
            (function_impl
             (Fn
              ((Expr
                (Value
                 (Type
                  (StructType
                   ((struct_fields
                     ((a ((field_type (ExprType (Reference (A (TypeN 0)))))))))
                    (struct_methods ()) (struct_impls ()) (struct_id 4))))))))))))))))) |}]

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
              (function_impl (Fn ((Block ((Break (Expr (Value (Integer 1))))))))))))))))) |}]

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
       ((b
         (Value
          (Struct
           (((struct_fields ((integer ((field_type IntegerType)))))
             (struct_methods
              ((new
                ((function_signature
                  ((function_params ((integer IntegerType)))
                   (function_returns SelfType)))
                 (function_impl (BuiltinFn (<fun> 4)))))
               (serialize
                ((function_signature
                  ((function_params ((self SelfType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder))))
                 (function_impl
                  (Fn
                   ((Return
                     (Primitive
                      (StoreInt (builder (Reference (b (BuiltinType Builder))))
                       (length (Value (Integer 257)))
                       (integer
                        (StructField ((Reference (self SelfType)) integer)))
                       (signed true)))))))))))
             (struct_impls ()) (struct_id 0))
            ((integer (Value (Integer 1))))))))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 4)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 257)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 0))))))
              (function_returns
               (StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 4)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 257)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 0))))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (Reference
                     (i
                      (StructType
                       ((struct_fields ((integer ((field_type IntegerType)))))
                        (struct_methods
                         ((new
                           ((function_signature
                             ((function_params ((integer IntegerType)))
                              (function_returns SelfType)))
                            (function_impl (BuiltinFn (<fun> 4)))))
                          (serialize
                           ((function_signature
                             ((function_params
                               ((self SelfType) (b (BuiltinType Builder))))
                              (function_returns (BuiltinType Builder))))
                            (function_impl
                             (Fn
                              ((Return
                                (Primitive
                                 (StoreInt
                                  (builder (Reference (b (BuiltinType Builder))))
                                  (length (Value (Integer 257)))
                                  (integer
                                   (StructField
                                    ((Reference (self SelfType)) integer)))
                                  (signed true)))))))))))
                        (struct_impls ()) (struct_id 0))))))))
                 (Break
                  (Expr
                   (Reference
                    (a
                     (StructType
                      ((struct_fields ((integer ((field_type IntegerType)))))
                       (struct_methods
                        ((new
                          ((function_signature
                            ((function_params ((integer IntegerType)))
                             (function_returns SelfType)))
                           (function_impl (BuiltinFn (<fun> 4)))))
                         (serialize
                          ((function_signature
                            ((function_params
                              ((self SelfType) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 257)))
                                 (integer
                                  (StructField
                                   ((Reference (self SelfType)) integer)))
                                 (signed true)))))))))))
                       (struct_impls ()) (struct_id 0))))))))))))))))))))
     |}]

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
             ((function_params
               ((x
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 4)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 257)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 0))))))
              (function_returns HoleType)))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference
                        (x
                         (StructType
                          ((struct_fields ((integer ((field_type IntegerType)))))
                           (struct_methods
                            ((new
                              ((function_signature
                                ((function_params ((integer IntegerType)))
                                 (function_returns SelfType)))
                               (function_impl (BuiltinFn (<fun> 4)))))
                             (serialize
                              ((function_signature
                                ((function_params
                                  ((self SelfType) (b (BuiltinType Builder))))
                                 (function_returns (BuiltinType Builder))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (Primitive
                                    (StoreInt
                                     (builder
                                      (Reference (b (BuiltinType Builder))))
                                     (length (Value (Integer 257)))
                                     (integer
                                      (StructField
                                       ((Reference (self SelfType)) integer)))
                                     (signed true)))))))))))
                           (struct_impls ()) (struct_id 0)))))
                       (Reference
                        (x
                         (StructType
                          ((struct_fields ((integer ((field_type IntegerType)))))
                           (struct_methods
                            ((new
                              ((function_signature
                                ((function_params ((integer IntegerType)))
                                 (function_returns SelfType)))
                               (function_impl (BuiltinFn (<fun> 4)))))
                             (serialize
                              ((function_signature
                                ((function_params
                                  ((self SelfType) (b (BuiltinType Builder))))
                                 (function_returns (BuiltinType Builder))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (Primitive
                                    (StoreInt
                                     (builder
                                      (Reference (b (BuiltinType Builder))))
                                     (length (Value (Integer 257)))
                                     (integer
                                      (StructField
                                       ((Reference (self SelfType)) integer)))
                                     (signed true)))))))))))
                           (struct_impls ()) (struct_id 0)))))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference
                        (a
                         (StructType
                          ((struct_fields ((integer ((field_type IntegerType)))))
                           (struct_methods
                            ((new
                              ((function_signature
                                ((function_params ((integer IntegerType)))
                                 (function_returns SelfType)))
                               (function_impl (BuiltinFn (<fun> 4)))))
                             (serialize
                              ((function_signature
                                ((function_params
                                  ((self SelfType) (b (BuiltinType Builder))))
                                 (function_returns (BuiltinType Builder))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (Primitive
                                    (StoreInt
                                     (builder
                                      (Reference (b (BuiltinType Builder))))
                                     (length (Value (Integer 257)))
                                     (integer
                                      (StructField
                                       ((Reference (self SelfType)) integer)))
                                     (signed true)))))))))))
                           (struct_impls ()) (struct_id 0)))))
                       (Reference
                        (a
                         (StructType
                          ((struct_fields ((integer ((field_type IntegerType)))))
                           (struct_methods
                            ((new
                              ((function_signature
                                ((function_params ((integer IntegerType)))
                                 (function_returns SelfType)))
                               (function_impl (BuiltinFn (<fun> 4)))))
                             (serialize
                              ((function_signature
                                ((function_params
                                  ((self SelfType) (b (BuiltinType Builder))))
                                 (function_returns (BuiltinType Builder))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (Primitive
                                    (StoreInt
                                     (builder
                                      (Reference (b (BuiltinType Builder))))
                                     (length (Value (Integer 257)))
                                     (integer
                                      (StructField
                                       ((Reference (self SelfType)) integer)))
                                     (signed true)))))))))))
                           (struct_impls ()) (struct_id 0))))))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 4)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 257)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 0))))
                (i_
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 4)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 257)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 0))))))
              (function_returns
               (StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 4)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 257)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 0))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (i
                     (StructType
                      ((struct_fields ((integer ((field_type IntegerType)))))
                       (struct_methods
                        ((new
                          ((function_signature
                            ((function_params ((integer IntegerType)))
                             (function_returns SelfType)))
                           (function_impl (BuiltinFn (<fun> 4)))))
                         (serialize
                          ((function_signature
                            ((function_params
                              ((self SelfType) (b (BuiltinType Builder))))
                             (function_returns (BuiltinType Builder))))
                           (function_impl
                            (Fn
                             ((Return
                               (Primitive
                                (StoreInt
                                 (builder (Reference (b (BuiltinType Builder))))
                                 (length (Value (Integer 257)))
                                 (integer
                                  (StructField
                                   ((Reference (self SelfType)) integer)))
                                 (signed true)))))))))))
                       (struct_impls ()) (struct_id 0)))))))))))))))))))) |}]

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
        (i (Value (Integer 1))))))) |}]

let%expect_test "method access" =
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
       ((res (Value (Integer 1)))
        (foo
         (Value
          (Struct
           (((struct_fields ())
             (struct_methods
              ((bar
                ((function_signature
                  ((function_params ((self SelfType) (i IntegerType)))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
             (struct_impls ()) (struct_id 5))
            ()))))
        (Foo
         (Value
          (Type
           (StructType
            ((struct_fields ())
             (struct_methods
              ((bar
                ((function_signature
                  ((function_params ((self SelfType) (i IntegerType)))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn ((Block ((Break (Expr (Reference (i IntegerType)))))))))))))
             (struct_impls ()) (struct_id 5)))))))))) |}]

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
     ((bindings
       ((Foo
         (Value
          (Type
           (StructType
            ((struct_fields ())
             (struct_methods
              ((bar
                ((function_signature
                  ((function_params ((self SelfType)))
                   (function_returns SelfType)))
                 (function_impl
                  (Fn ((Block ((Break (Expr (Reference (self SelfType)))))))))))))
             (struct_impls ()) (struct_id 6)))))))))) |}]

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
         (Value
          (Struct
           (((struct_fields
              ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
             (struct_methods ()) (struct_impls ()) (struct_id 7))
            ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T
         (Value
          (Type
           (StructType
            ((struct_fields
              ((a ((field_type IntegerType))) (b ((field_type IntegerType)))))
             (struct_methods ()) (struct_impls ()) (struct_id 7)))))))))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError
        ((StructType
          ((struct_fields ((integer ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((integer IntegerType)))
                 (function_returns SelfType)))
               (function_impl (BuiltinFn (<fun> 9)))))
             (serialize
              ((function_signature
                ((function_params ((self SelfType) (b (BuiltinType Builder))))
                 (function_returns (BuiltinType Builder))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreInt (builder (Reference (b (BuiltinType Builder))))
                     (length (Value (Integer 64)))
                     (integer
                      (StructField ((Reference (self SelfType)) integer)))
                     (signed true)))))))))))
           (struct_impls ()) (struct_id 9)))
         (StructType
          ((struct_fields ((integer ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((integer IntegerType)))
                 (function_returns SelfType)))
               (function_impl (BuiltinFn (<fun> 8)))))
             (serialize
              ((function_signature
                ((function_params ((self SelfType) (b (BuiltinType Builder))))
                 (function_returns (BuiltinType Builder))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreInt (builder (Reference (b (BuiltinType Builder))))
                     (length (Value (Integer 32)))
                     (integer
                      (StructField ((Reference (self SelfType)) integer)))
                     (signed true)))))))))))
           (struct_impls ()) (struct_id 8)))))
       ((bindings
         ((foo
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((i
                   (StructType
                    ((struct_fields ((integer ((field_type IntegerType)))))
                     (struct_methods
                      ((new
                        ((function_signature
                          ((function_params ((integer IntegerType)))
                           (function_returns SelfType)))
                         (function_impl (BuiltinFn (<fun> 8)))))
                       (serialize
                        ((function_signature
                          ((function_params
                            ((self SelfType) (b (BuiltinType Builder))))
                           (function_returns (BuiltinType Builder))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder (Reference (b (BuiltinType Builder))))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference (self SelfType)) integer)))
                               (signed true)))))))))))
                     (struct_impls ()) (struct_id 8))))))
                (function_returns
                 (StructType
                  ((struct_fields ((integer ((field_type IntegerType)))))
                   (struct_methods
                    ((new
                      ((function_signature
                        ((function_params ((integer IntegerType)))
                         (function_returns SelfType)))
                       (function_impl (BuiltinFn (<fun> 9)))))
                     (serialize
                      ((function_signature
                        ((function_params
                          ((self SelfType) (b (BuiltinType Builder))))
                         (function_returns (BuiltinType Builder))))
                       (function_impl
                        (Fn
                         ((Return
                           (Primitive
                            (StoreInt
                             (builder (Reference (b (BuiltinType Builder))))
                             (length (Value (Integer 64)))
                             (integer
                              (StructField ((Reference (self SelfType)) integer)))
                             (signed true)))))))))))
                   (struct_impls ()) (struct_id 9))))))
              (function_impl
               (Fn
                ((Block
                  ((Return
                    (Reference
                     (i
                      (StructType
                       ((struct_fields ((integer ((field_type IntegerType)))))
                        (struct_methods
                         ((new
                           ((function_signature
                             ((function_params ((integer IntegerType)))
                              (function_returns SelfType)))
                            (function_impl (BuiltinFn (<fun> 8)))))
                          (serialize
                           ((function_signature
                             ((function_params
                               ((self SelfType) (b (BuiltinType Builder))))
                              (function_returns (BuiltinType Builder))))
                            (function_impl
                             (Fn
                              ((Return
                                (Primitive
                                 (StoreInt
                                  (builder (Reference (b (BuiltinType Builder))))
                                  (length (Value (Integer 32)))
                                  (integer
                                   (StructField
                                    ((Reference (self SelfType)) integer)))
                                  (signed true)))))))))))
                        (struct_impls ()) (struct_id 8)))))))))))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

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
            (function_impl (Fn ((Block ((Return (Reference (i IntegerType)))))))))))))))) |}]

let%expect_test "scope doesn't leak bindings" =
  let source = {|
    {
     let a = 1;
    }
  |} in
  pp source ; [%expect {| (Ok ((bindings ()))) |}]

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
            (function_impl (Fn ((Block ((Break (Expr (Value (Bool true))))))))))))))))) |}]

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
     (((TypeError
        ((StructType
          ((struct_fields ((integer ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((integer IntegerType)))
                 (function_returns SelfType)))
               (function_impl (BuiltinFn (<fun> 10)))))
             (serialize
              ((function_signature
                ((function_params ((self SelfType) (b (BuiltinType Builder))))
                 (function_returns (BuiltinType Builder))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreInt (builder (Reference (b (BuiltinType Builder))))
                     (length (Value (Integer 99)))
                     (integer
                      (StructField ((Reference (self SelfType)) integer)))
                     (signed true)))))))))))
           (struct_impls ()) (struct_id 10)))
         (StructType
          ((struct_fields ((integer ((field_type IntegerType)))))
           (struct_methods
            ((new
              ((function_signature
                ((function_params ((integer IntegerType)))
                 (function_returns SelfType)))
               (function_impl (BuiltinFn (<fun> 11)))))
             (serialize
              ((function_signature
                ((function_params ((self SelfType) (b (BuiltinType Builder))))
                 (function_returns (BuiltinType Builder))))
               (function_impl
                (Fn
                 ((Return
                   (Primitive
                    (StoreInt (builder (Reference (b (BuiltinType Builder))))
                     (length (Value (Integer 10)))
                     (integer
                      (StructField ((Reference (self SelfType)) integer)))
                     (signed true)))))))))))
           (struct_impls ()) (struct_id 11)))))
       ((bindings
         ((Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

let%expect_test "implement interface op" =
  let source =
    {|
      struct Left {
        // built-in interface
        impl BinOp {
          fn op(left: Integer, right: Integer) -> Integer { left }
        }
      }
      let one = Left.op(1, 2);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((one (Value (Integer 1)))
        (Left
         (Value
          (Type
           (StructType
            ((struct_fields ())
             (struct_methods
              ((op
                ((function_signature
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType)))
                 (function_impl
                  (Fn ((Block ((Break (Expr (Reference (left IntegerType)))))))))))))
             (struct_impls
              (((impl_interface
                 (Value
                  (Type
                   (InterfaceType
                    ((interface_methods
                      ((op
                        ((function_params
                          ((left IntegerType) (right IntegerType)))
                         (function_returns IntegerType))))))))))
                (impl_methods
                 ((op
                   (Value
                    (Function
                     ((function_signature
                       ((function_params
                         ((left IntegerType) (right IntegerType)))
                        (function_returns IntegerType)))
                      (function_impl
                       (Fn
                        ((Block ((Break (Expr (Reference (left IntegerType))))))))))))))))))
             (struct_id 12)))))))))) |}]

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
    (Error
     (((UnexpectedTypeSC SelfType)
       ((bindings
         ((empty
           (Value
            (Struct
             (((struct_fields ()) (struct_methods ()) (struct_impls ())
               (struct_id 0))
              ()))))
          (Empty
           (Value
            (Type
             (StructType
              ((struct_fields ())
               (struct_methods
                ((new
                  ((function_signature
                    ((function_params ()) (function_returns SelfType)))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct
                            (((struct_fields ()) (struct_methods ())
                              (struct_impls ()) (struct_id 0))
                             ()))))))))))))))
               (struct_impls
                (((impl_interface
                   (Value
                    (Type
                     (InterfaceType
                      ((interface_methods
                        ((new ((function_params ()) (function_returns SelfType))))))))))
                  (impl_methods
                   ((new
                     (Value
                      (Function
                       ((function_signature
                         ((function_params ()) (function_returns SelfType)))
                        (function_impl
                         (Fn
                          ((Block
                            ((Break
                              (Expr
                               (Value
                                (Struct
                                 (((struct_fields ()) (struct_methods ())
                                   (struct_impls ()) (struct_id 0))
                                  ())))))))))))))))))))
               (struct_id 13))))))
          (Make
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((new ((function_params ()) (function_returns SelfType))))))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2)))))))))))
      ((TypeError
        (SelfType
         (StructType
          ((struct_fields ()) (struct_methods ()) (struct_impls ())
           (struct_id 0)))))
       ((bindings
         ((empty
           (Value
            (Struct
             (((struct_fields ()) (struct_methods ()) (struct_impls ())
               (struct_id 0))
              ()))))
          (Empty
           (Value
            (Type
             (StructType
              ((struct_fields ())
               (struct_methods
                ((new
                  ((function_signature
                    ((function_params ()) (function_returns SelfType)))
                   (function_impl
                    (Fn
                     ((Block
                       ((Break
                         (Expr
                          (Value
                           (Struct
                            (((struct_fields ()) (struct_methods ())
                              (struct_impls ()) (struct_id 0))
                             ()))))))))))))))
               (struct_impls
                (((impl_interface
                   (Value
                    (Type
                     (InterfaceType
                      ((interface_methods
                        ((new ((function_params ()) (function_returns SelfType))))))))))
                  (impl_methods
                   ((new
                     (Value
                      (Function
                       ((function_signature
                         ((function_params ()) (function_returns SelfType)))
                        (function_impl
                         (Fn
                          ((Block
                            ((Break
                              (Expr
                               (Value
                                (Struct
                                 (((struct_fields ()) (struct_methods ())
                                   (struct_impls ()) (struct_id 0))
                                  ())))))))))))))))))))
               (struct_id 13))))))
          (Make
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((new ((function_params ()) (function_returns SelfType))))))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

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
             ((function_params
               ((self
                 (StructType
                  ((struct_fields
                    ((y
                      ((field_type
                        (StructType
                         ((struct_fields ((integer ((field_type IntegerType)))))
                          (struct_methods
                           ((new
                             ((function_signature
                               ((function_params ((integer IntegerType)))
                                (function_returns SelfType)))
                              (function_impl (BuiltinFn (<fun> 8)))))
                            (serialize
                             ((function_signature
                               ((function_params
                                 ((self SelfType) (b (BuiltinType Builder))))
                                (function_returns (BuiltinType Builder))))
                              (function_impl
                               (Fn
                                ((Return
                                  (Primitive
                                   (StoreInt
                                    (builder
                                     (Reference (b (BuiltinType Builder))))
                                    (length (Value (Integer 32)))
                                    (integer
                                     (StructField
                                      ((Reference (self SelfType)) integer)))
                                    (signed true)))))))))))
                          (struct_impls ()) (struct_id 8))))))
                     (z
                      ((field_type
                        (StructType
                         ((struct_fields
                           ((x
                             ((field_type
                               (StructType
                                ((struct_fields
                                  ((integer ((field_type IntegerType)))))
                                 (struct_methods
                                  ((new
                                    ((function_signature
                                      ((function_params ((integer IntegerType)))
                                       (function_returns SelfType)))
                                     (function_impl (BuiltinFn (<fun> 8)))))
                                   (serialize
                                    ((function_signature
                                      ((function_params
                                        ((self SelfType)
                                         (b (BuiltinType Builder))))
                                       (function_returns (BuiltinType Builder))))
                                     (function_impl
                                      (Fn
                                       ((Return
                                         (Primitive
                                          (StoreInt
                                           (builder
                                            (Reference (b (BuiltinType Builder))))
                                           (length (Value (Integer 32)))
                                           (integer
                                            (StructField
                                             ((Reference (self SelfType))
                                              integer)))
                                           (signed true)))))))))))
                                 (struct_impls ()) (struct_id 8))))))))
                          (struct_methods ()) (struct_impls ()) (struct_id 14))))))))
                   (struct_methods ()) (struct_impls ()) (struct_id 15))))
                (b (BuiltinType Builder))))
              (function_returns (BuiltinType Builder))))
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
                            ((self SelfType) (b (BuiltinType Builder))))
                           (function_returns (BuiltinType Builder))))
                         (function_impl
                          (Fn
                           ((Return
                             (Primitive
                              (StoreInt
                               (builder (Reference (b (BuiltinType Builder))))
                               (length (Value (Integer 32)))
                               (integer
                                (StructField
                                 ((Reference (self SelfType)) integer)))
                               (signed true))))))))))
                      ((StructField
                        ((Reference
                          (self
                           (StructType
                            ((struct_fields
                              ((y
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((integer ((field_type IntegerType)))))
                                    (struct_methods
                                     ((new
                                       ((function_signature
                                         ((function_params
                                           ((integer IntegerType)))
                                          (function_returns SelfType)))
                                        (function_impl (BuiltinFn (<fun> 8)))))
                                      (serialize
                                       ((function_signature
                                         ((function_params
                                           ((self SelfType)
                                            (b (BuiltinType Builder))))
                                          (function_returns
                                           (BuiltinType Builder))))
                                        (function_impl
                                         (Fn
                                          ((Return
                                            (Primitive
                                             (StoreInt
                                              (builder
                                               (Reference
                                                (b (BuiltinType Builder))))
                                              (length (Value (Integer 32)))
                                              (integer
                                               (StructField
                                                ((Reference (self SelfType))
                                                 integer)))
                                              (signed true)))))))))))
                                    (struct_impls ()) (struct_id 8))))))
                               (z
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((x
                                       ((field_type
                                         (StructType
                                          ((struct_fields
                                            ((integer ((field_type IntegerType)))))
                                           (struct_methods
                                            ((new
                                              ((function_signature
                                                ((function_params
                                                  ((integer IntegerType)))
                                                 (function_returns SelfType)))
                                               (function_impl
                                                (BuiltinFn (<fun> 8)))))
                                             (serialize
                                              ((function_signature
                                                ((function_params
                                                  ((self SelfType)
                                                   (b (BuiltinType Builder))))
                                                 (function_returns
                                                  (BuiltinType Builder))))
                                               (function_impl
                                                (Fn
                                                 ((Return
                                                   (Primitive
                                                    (StoreInt
                                                     (builder
                                                      (Reference
                                                       (b (BuiltinType Builder))))
                                                     (length
                                                      (Value (Integer 32)))
                                                     (integer
                                                      (StructField
                                                       ((Reference
                                                         (self SelfType))
                                                        integer)))
                                                     (signed true)))))))))))
                                           (struct_impls ()) (struct_id 8))))))))
                                    (struct_methods ()) (struct_impls ())
                                    (struct_id 14))))))))
                             (struct_methods ()) (struct_impls ())
                             (struct_id 15)))))
                         y))
                       (Reference (b (BuiltinType Builder)))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((Value
                       (Function
                        ((function_signature
                          ((function_params
                            ((self
                              (StructType
                               ((struct_fields
                                 ((x
                                   ((field_type
                                     (StructType
                                      ((struct_fields
                                        ((integer ((field_type IntegerType)))))
                                       (struct_methods
                                        ((new
                                          ((function_signature
                                            ((function_params
                                              ((integer IntegerType)))
                                             (function_returns SelfType)))
                                           (function_impl (BuiltinFn (<fun> 8)))))
                                         (serialize
                                          ((function_signature
                                            ((function_params
                                              ((self SelfType)
                                               (b (BuiltinType Builder))))
                                             (function_returns
                                              (BuiltinType Builder))))
                                           (function_impl
                                            (Fn
                                             ((Return
                                               (Primitive
                                                (StoreInt
                                                 (builder
                                                  (Reference
                                                   (b (BuiltinType Builder))))
                                                 (length (Value (Integer 32)))
                                                 (integer
                                                  (StructField
                                                   ((Reference (self SelfType))
                                                    integer)))
                                                 (signed true)))))))))))
                                       (struct_impls ()) (struct_id 8))))))))
                                (struct_methods ()) (struct_impls ())
                                (struct_id 14))))
                             (b (BuiltinType Builder))))
                           (function_returns (BuiltinType Builder))))
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
                                         ((self SelfType)
                                          (b (BuiltinType Builder))))
                                        (function_returns (BuiltinType Builder))))
                                      (function_impl
                                       (Fn
                                        ((Return
                                          (Primitive
                                           (StoreInt
                                            (builder
                                             (Reference
                                              (b (BuiltinType Builder))))
                                            (length (Value (Integer 32)))
                                            (integer
                                             (StructField
                                              ((Reference (self SelfType))
                                               integer)))
                                            (signed true))))))))))
                                   ((StructField
                                     ((Reference
                                       (self
                                        (StructType
                                         ((struct_fields
                                           ((x
                                             ((field_type
                                               (StructType
                                                ((struct_fields
                                                  ((integer
                                                    ((field_type IntegerType)))))
                                                 (struct_methods
                                                  ((new
                                                    ((function_signature
                                                      ((function_params
                                                        ((integer IntegerType)))
                                                       (function_returns
                                                        SelfType)))
                                                     (function_impl
                                                      (BuiltinFn (<fun> 8)))))
                                                   (serialize
                                                    ((function_signature
                                                      ((function_params
                                                        ((self SelfType)
                                                         (b
                                                          (BuiltinType Builder))))
                                                       (function_returns
                                                        (BuiltinType Builder))))
                                                     (function_impl
                                                      (Fn
                                                       ((Return
                                                         (Primitive
                                                          (StoreInt
                                                           (builder
                                                            (Reference
                                                             (b
                                                              (BuiltinType
                                                               Builder))))
                                                           (length
                                                            (Value (Integer 32)))
                                                           (integer
                                                            (StructField
                                                             ((Reference
                                                               (self SelfType))
                                                              integer)))
                                                           (signed true)))))))))))
                                                 (struct_impls ()) (struct_id 8))))))))
                                          (struct_methods ()) (struct_impls ())
                                          (struct_id 14)))))
                                      x))
                                    (Reference (b (BuiltinType Builder)))))))))
                              (Return (Reference (b (BuiltinType Builder))))))))))))
                      ((StructField
                        ((Reference
                          (self
                           (StructType
                            ((struct_fields
                              ((y
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((integer ((field_type IntegerType)))))
                                    (struct_methods
                                     ((new
                                       ((function_signature
                                         ((function_params
                                           ((integer IntegerType)))
                                          (function_returns SelfType)))
                                        (function_impl (BuiltinFn (<fun> 8)))))
                                      (serialize
                                       ((function_signature
                                         ((function_params
                                           ((self SelfType)
                                            (b (BuiltinType Builder))))
                                          (function_returns
                                           (BuiltinType Builder))))
                                        (function_impl
                                         (Fn
                                          ((Return
                                            (Primitive
                                             (StoreInt
                                              (builder
                                               (Reference
                                                (b (BuiltinType Builder))))
                                              (length (Value (Integer 32)))
                                              (integer
                                               (StructField
                                                ((Reference (self SelfType))
                                                 integer)))
                                              (signed true)))))))))))
                                    (struct_impls ()) (struct_id 8))))))
                               (z
                                ((field_type
                                  (StructType
                                   ((struct_fields
                                     ((x
                                       ((field_type
                                         (StructType
                                          ((struct_fields
                                            ((integer ((field_type IntegerType)))))
                                           (struct_methods
                                            ((new
                                              ((function_signature
                                                ((function_params
                                                  ((integer IntegerType)))
                                                 (function_returns SelfType)))
                                               (function_impl
                                                (BuiltinFn (<fun> 8)))))
                                             (serialize
                                              ((function_signature
                                                ((function_params
                                                  ((self SelfType)
                                                   (b (BuiltinType Builder))))
                                                 (function_returns
                                                  (BuiltinType Builder))))
                                               (function_impl
                                                (Fn
                                                 ((Return
                                                   (Primitive
                                                    (StoreInt
                                                     (builder
                                                      (Reference
                                                       (b (BuiltinType Builder))))
                                                     (length
                                                      (Value (Integer 32)))
                                                     (integer
                                                      (StructField
                                                       ((Reference
                                                         (self SelfType))
                                                        integer)))
                                                     (signed true)))))))))))
                                           (struct_impls ()) (struct_id 8))))))))
                                    (struct_methods ()) (struct_impls ())
                                    (struct_id 14))))))))
                             (struct_methods ()) (struct_impls ())
                             (struct_id 15)))))
                         z))
                       (Reference (b (BuiltinType Builder)))))))))
                 (Return (Reference (b (BuiltinType Builder)))))))))))))
        (Outer
         (Value
          (Type
           (StructType
            ((struct_fields
              ((y
                ((field_type
                  (StructType
                   ((struct_fields ((integer ((field_type IntegerType)))))
                    (struct_methods
                     ((new
                       ((function_signature
                         ((function_params ((integer IntegerType)))
                          (function_returns SelfType)))
                        (function_impl (BuiltinFn (<fun> 8)))))
                      (serialize
                       ((function_signature
                         ((function_params
                           ((self SelfType) (b (BuiltinType Builder))))
                          (function_returns (BuiltinType Builder))))
                        (function_impl
                         (Fn
                          ((Return
                            (Primitive
                             (StoreInt
                              (builder (Reference (b (BuiltinType Builder))))
                              (length (Value (Integer 32)))
                              (integer
                               (StructField
                                ((Reference (self SelfType)) integer)))
                              (signed true)))))))))))
                    (struct_impls ()) (struct_id 8))))))
               (z
                ((field_type
                  (StructType
                   ((struct_fields
                     ((x
                       ((field_type
                         (StructType
                          ((struct_fields ((integer ((field_type IntegerType)))))
                           (struct_methods
                            ((new
                              ((function_signature
                                ((function_params ((integer IntegerType)))
                                 (function_returns SelfType)))
                               (function_impl (BuiltinFn (<fun> 8)))))
                             (serialize
                              ((function_signature
                                ((function_params
                                  ((self SelfType) (b (BuiltinType Builder))))
                                 (function_returns (BuiltinType Builder))))
                               (function_impl
                                (Fn
                                 ((Return
                                   (Primitive
                                    (StoreInt
                                     (builder
                                      (Reference (b (BuiltinType Builder))))
                                     (length (Value (Integer 32)))
                                     (integer
                                      (StructField
                                       ((Reference (self SelfType)) integer)))
                                     (signed true)))))))))))
                           (struct_impls ()) (struct_id 8))))))))
                    (struct_methods ()) (struct_impls ()) (struct_id 14))))))))
             (struct_methods ()) (struct_impls ()) (struct_id 15))))))
        (Inner
         (Value
          (Type
           (StructType
            ((struct_fields
              ((x
                ((field_type
                  (StructType
                   ((struct_fields ((integer ((field_type IntegerType)))))
                    (struct_methods
                     ((new
                       ((function_signature
                         ((function_params ((integer IntegerType)))
                          (function_returns SelfType)))
                        (function_impl (BuiltinFn (<fun> 8)))))
                      (serialize
                       ((function_signature
                         ((function_params
                           ((self SelfType) (b (BuiltinType Builder))))
                          (function_returns (BuiltinType Builder))))
                        (function_impl
                         (Fn
                          ((Return
                            (Primitive
                             (StoreInt
                              (builder (Reference (b (BuiltinType Builder))))
                              (length (Value (Integer 32)))
                              (integer
                               (StructField
                                ((Reference (self SelfType)) integer)))
                              (signed true)))))))))))
                    (struct_impls ()) (struct_id 8))))))))
             (struct_methods ()) (struct_impls ()) (struct_id 14)))))))))) |}]

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
                   (Value
                    (Function
                     ((function_signature
                       ((function_params
                         ((x (ExprType (Reference (X (TypeN 0)))))))
                        (function_returns (ExprType (Reference (X (TypeN 0)))))))
                      (function_impl
                       (Fn
                        ((Block
                          ((Break
                            (Expr
                             (Reference (x (ExprType (Reference (X (TypeN 0))))))))))))))))))))))))))))))) |}]

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
                    (Value
                     (Function
                      ((function_signature
                        ((function_params
                          ((x (ExprType (Reference (X (TypeN 0)))))))
                         (function_returns (ExprType (Reference (X (TypeN 0)))))))
                       (function_impl
                        (Fn
                         ((Block
                           ((Break
                             (Expr
                              (Reference
                               (x (ExprType (Reference (X (TypeN 0)))))))))))))))))))
                 (Break (Expr (ResolvedReference (f <opaque>))))))))))))))))) |}]

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
               (Fn ((Block ((Break (Expr (Reference (X (TypeN 0))))))))))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits IntegerType)))
                (function_returns (TypeN 0))))
              (function_impl (BuiltinFn (<fun> 0)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type (TypeN 0))))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (TypeN 0))))
                (function_returns
                 (FunctionType
                  ((function_params ((t HoleType) (b (BuiltinType Builder))))
                   (function_returns (BuiltinType Builder)))))))
              (function_impl (BuiltinFn (<fun> 1)))))))
          (BinOp
           (Value
            (Type
             (InterfaceType
              ((interface_methods
                ((op
                  ((function_params ((left IntegerType) (right IntegerType)))
                   (function_returns IntegerType))))))))))
          (From
           (Value
            (Function
             ((function_signature
               ((function_params ((T (TypeN 0)))) (function_returns HoleType)))
              (function_impl (BuiltinFn (<fun> 2))))))))))))) |}]

let%expect_test "unions" =
  let source =
    {|
      union Test {
        case Int(257)
        case Int(64)
      }
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((Test
         (Value
          (Type
           (UnionType
            ((cases
              ((StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 4)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 257)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 0)))
               (StructType
                ((struct_fields ((integer ((field_type IntegerType)))))
                 (struct_methods
                  ((new
                    ((function_signature
                      ((function_params ((integer IntegerType)))
                       (function_returns SelfType)))
                     (function_impl (BuiltinFn (<fun> 9)))))
                   (serialize
                    ((function_signature
                      ((function_params
                        ((self SelfType) (b (BuiltinType Builder))))
                       (function_returns (BuiltinType Builder))))
                     (function_impl
                      (Fn
                       ((Return
                         (Primitive
                          (StoreInt
                           (builder (Reference (b (BuiltinType Builder))))
                           (length (Value (Integer 64)))
                           (integer
                            (StructField ((Reference (self SelfType)) integer)))
                           (signed true)))))))))))
                 (struct_impls ()) (struct_id 9)))))))))))))) |}]
