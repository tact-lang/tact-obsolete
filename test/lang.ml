open Shared
open Tact.Lang_types

let incr_f =
  Function
    { function_signature =
        { function_params = [("value", Value (Type IntegerType))];
          function_returns = Value (Type IntegerType) };
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
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))
        (T
         (Value
          (Type
           (StructType
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((UnresolvedIdentifier Int256)
       ((stmts ((Let ((T (Reference (Int256 (Value (Type HoleType)))))))))
        (bindings
         ((Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits (Value (Type IntegerType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type TypeType)))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (Value (Type TypeType)))))
                (function_returns
                 (Value
                  (Type
                   (FunctionType
                    ((function_params
                      ((t (Value (Type HoleType)))
                       (builder (Value (Type (BuiltinType Builder))))))
                     (function_returns (Value (Type VoidType))))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (methods
         (((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn ((Return (Primitive EmptyBuilder))))))))))))))) |}]

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
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))
        (A
         (Value
          (Type
           (StructType
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
                  (Value
                   (Type
                    (StructType
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>))))))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields
             ((t
               ((field_type
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))))
            (struct_id <opaque>))))
         ())
        ((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
    let a = test(test(1));
  |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((a (Value (Integer 1)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (i
                     (Value
                      (Type
                       (StructType
                        ((struct_fields
                          ((integer ((field_type (Value (Type IntegerType)))))))
                         (struct_id <opaque>))))))))))))))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
             ((function_params ()) (function_returns (Value (Type IntegerType)))))
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
                    (Value
                     (Type
                      (StructType
                       ((struct_fields
                         ((integer ((field_type (Value (Type IntegerType)))))))
                        (struct_id <opaque>))))))))
                 (b ((field_type (Value (Type BoolType)))))))
               (struct_id <opaque>))))))))
        (methods
         (((Type
            (StructType
             ((struct_fields
               ((a
                 ((field_type
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))))
                (b ((field_type (Value (Type BoolType)))))))
              (struct_id <opaque>))))
           ())
          ((Type
            (StructType
             ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>))))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))
                  (builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (Reference (builder (Value (Type (BuiltinType Builder))))))
                    (length (Value (Integer 257)))
                    (integer
                     (StructField
                      ((Reference
                        (self
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       integer)))
                    (signed true))))))))))))))) |}]

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
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (a ((field_type (Value (Type BoolType)))))))
          (struct_id <opaque>))))
       ((stmts
         ((Let
           ((MyType
             (Value
              (Type
               (StructType
                ((struct_fields
                  ((a
                    ((field_type
                      (Value
                       (Type
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>))))))))
                   (a ((field_type (Value (Type BoolType)))))))
                 (struct_id <opaque>))))))))))
        (bindings
         ((MyType
           (Value
            (Type
             (StructType
              ((struct_fields
                ((a
                  ((field_type
                    (Value
                     (Type
                      (StructType
                       ((struct_fields
                         ((integer ((field_type (Value (Type IntegerType)))))))
                        (struct_id <opaque>))))))))
                 (a ((field_type (Value (Type BoolType)))))))
               (struct_id <opaque>))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits (Value (Type IntegerType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type TypeType)))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (Value (Type TypeType)))))
                (function_returns
                 (Value
                  (Type
                   (FunctionType
                    ((function_params
                      ((t (Value (Type HoleType)))
                       (builder (Value (Type (BuiltinType Builder))))))
                     (function_returns (Value (Type VoidType))))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (methods
         (((Type
            (StructType
             ((struct_fields
               ((a
                 ((field_type
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))))
                (a ((field_type (Value (Type BoolType)))))))
              (struct_id <opaque>))))
           ())
          ((Type
            (StructType
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>))))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))
                  (builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (Reference (builder (Value (Type (BuiltinType Builder))))))
                    (length (Value (Integer 257)))
                    (integer
                     (StructField
                      ((Reference
                        (self
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       integer)))
                    (signed true)))))))))))
          ((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn ((Return (Primitive EmptyBuilder))))))))))))))) |}]

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
                  (Value
                   (Type
                    (StructType
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>))))))))))
             (struct_id <opaque>))))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ((A (Value (Type TypeType)))))
              (function_returns (Value (Type TypeType)))))
            (function_impl
             (Fn
              ((Expr
                (Value
                 (Type
                  (StructType
                   ((struct_fields
                     ((a ((field_type (Reference (A (Value (Type TypeType)))))))))
                    (struct_id <opaque>)))))))))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true)))))))))))
        ((Type
          (StructType
           ((struct_fields
             ((a ((field_type (Reference (A (Value (Type TypeType)))))))))
            (struct_id <opaque>))))
         ()))))) |}]

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
               ((function_params ()) (function_returns (Value (Type IntegerType)))))
              (function_impl (Fn ((Block ((Break (Expr (Value (Integer 1))))))))))))))))) |}]

let%expect_test "scoping that `let` introduces in code" =
  let source =
    {|
    fn f(i: Int(257)) {
      let a = i;
      a
    }
    let b = f(1);
    |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((bindings
       ((b (Value (Integer 1)))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (Reference
                     (i
                      (Value
                       (Type
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>))))))))))
                 (Break
                  (Expr
                   (Reference
                    (a
                     (Value
                      (Type
                       (StructType
                        ((struct_fields
                          ((integer ((field_type (Value (Type IntegerType)))))))
                         (struct_id <opaque>))))))))))))))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true)))))))))))))))
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
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_returns (Value (Type HoleType)))))
            (function_impl
             (Fn
              ((Block
                ((Let
                  ((a
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference
                        (x
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       (Reference
                        (x
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))))))))
                 (Let
                  ((b
                    (FunctionCall
                     ((ResolvedReference (op <opaque>))
                      ((Reference
                        (a
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       (Reference
                        (a
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>))))))))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (i_
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (i
                     (Value
                      (Type
                       (StructType
                        ((struct_fields
                          ((integer ((field_type (Value (Type IntegerType)))))))
                         (struct_id <opaque>))))))))))))))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Type
                 (StructType
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>))))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))
                (builder (Value (Type (BuiltinType Builder))))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              ((Return
                (Primitive
                 (StoreInt
                  (builder
                   (Reference (builder (Value (Type (BuiltinType Builder))))))
                  (length (Value (Integer 257)))
                  (integer
                   (StructField
                    ((Reference
                      (self
                       (Value
                        (Type
                         (StructType
                          ((struct_fields
                            ((integer ((field_type (Value (Type IntegerType)))))))
                           (struct_id <opaque>)))))))
                     integer)))
                  (signed true))))))))))))))) |}]

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
             ((function_params ()) (function_returns (Value (Type IntegerType)))))
            (function_impl
             (Fn ((Block ((Break (Expr (ResolvedReference (i <opaque>)))))))))))))
        (i (Value (Integer 1))))))) |}]

let%expect_test "method access" =
  let source =
    {|
      struct Foo {
        fn bar(self: Type, i: Integer) {
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
         (Value (StructInstance (((struct_fields ()) (struct_id <opaque>)) ()))))
        (Foo
         (Value (Type (StructType ((struct_fields ()) (struct_id <opaque>))))))))
      (methods
       (((Type (StructType ((struct_fields ()) (struct_id <opaque>))))
         ((bar
           ((function_signature
             ((function_params
               ((self (Value (Type TypeType))) (i (Value (Type IntegerType)))))
              (function_returns (Value (Type IntegerType)))))
            (function_impl
             (Fn
              ((Block
                ((Break (Expr (Reference (i (Value (Type IntegerType))))))))))))))))))) |}]

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
         (Value (Type (StructType ((struct_fields ()) (struct_id <opaque>))))))))
      (methods
       (((Type (StructType ((struct_fields ()) (struct_id <opaque>))))
         ((bar
           ((function_signature
             ((function_params
               ((self
                 (Value
                  (Type (StructType ((struct_fields ()) (struct_id <opaque>))))))))
              (function_returns
               (Value
                (Type (StructType ((struct_fields ()) (struct_id <opaque>))))))))
            (function_impl
             (Fn
              ((Block
                ((Break
                  (Expr
                   (Reference
                    (self
                     (Value
                      (Type
                       (StructType ((struct_fields ()) (struct_id <opaque>)))))))))))))))))))))) |}]

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
          (StructInstance
           (((struct_fields
              ((a ((field_type (Value (Type IntegerType)))))
               (b ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))
            ((a (Value (Integer 1))) (b (Value (Integer 2))))))))
        (T
         (Value
          (Type
           (StructType
            ((struct_fields
              ((a ((field_type (Value (Type IntegerType)))))
               (b ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>))))))))
      (methods
       (((Type
          (StructType
           ((struct_fields
             ((a ((field_type (Value (Type IntegerType)))))
              (b ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>))))
         ()))))) |}]

let%expect_test "type check error" =
  let source = {|
    fn foo(i: Int(32)) -> Int(64) { return i; }
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((TypeError
        ((Value
          (Type
           (StructType
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))))
         (Value
          (Type
           (StructType
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))))))
       ((stmts
         ((Let
           ((foo
             (Value
              (Function
               ((function_signature
                 ((function_params
                   ((i
                     (Value
                      (Type
                       (StructType
                        ((struct_fields
                          ((integer ((field_type (Value (Type IntegerType)))))))
                         (struct_id <opaque>))))))))
                  (function_returns
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))))
                (function_impl
                 (Fn
                  ((Block
                    ((Return
                      (Reference
                       (i
                        (Value
                         (Type
                          (StructType
                           ((struct_fields
                             ((integer ((field_type (Value (Type IntegerType)))))))
                            (struct_id <opaque>)))))))))))))))))))))
        (bindings
         ((foo
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((i
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl
               (Fn
                ((Block
                  ((Return
                    (Reference
                     (i
                      (Value
                       (Type
                        (StructType
                         ((struct_fields
                           ((integer ((field_type (Value (Type IntegerType)))))))
                          (struct_id <opaque>)))))))))))))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits (Value (Type IntegerType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Type BoolType))) (Type (Value (Type TypeType)))
          (Void (Value Void))
          (serializer
           (Value
            (Function
             ((function_signature
               ((function_params ((t (Value (Type TypeType)))))
                (function_returns
                 (Value
                  (Type
                   (FunctionType
                    ((function_params
                      ((t (Value (Type HoleType)))
                       (builder (Value (Type (BuiltinType Builder))))))
                     (function_returns (Value (Type VoidType))))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (methods
         (((Type
            (StructType
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>))))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))
                  (builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (Reference (builder (Value (Type (BuiltinType Builder))))))
                    (length (Value (Integer 64)))
                    (integer
                     (StructField
                      ((Reference
                        (self
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       integer)))
                    (signed true)))))))))))
          ((Type
            (StructType
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>))))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Type
                   (StructType
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>))))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((self
                   (Value
                    (Type
                     (StructType
                      ((struct_fields
                        ((integer ((field_type (Value (Type IntegerType)))))))
                       (struct_id <opaque>))))))
                  (builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                ((Return
                  (Primitive
                   (StoreInt
                    (builder
                     (Reference (builder (Value (Type (BuiltinType Builder))))))
                    (length (Value (Integer 32)))
                    (integer
                     (StructField
                      ((Reference
                        (self
                         (Value
                          (Type
                           (StructType
                            ((struct_fields
                              ((integer
                                ((field_type (Value (Type IntegerType)))))))
                             (struct_id <opaque>)))))))
                       integer)))
                    (signed true)))))))))))
          ((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn ((Return (Primitive EmptyBuilder))))))))))))))) |}]

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
             ((function_params ((i (Value (Type IntegerType)))))
              (function_returns (Value (Type IntegerType)))))
            (function_impl
             (Fn ((Block ((Return (Reference (i (Value (Type IntegerType)))))))))))))))))) |}]

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
             ((function_params ()) (function_returns (Value (Type IntegerType)))))
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
             ((function_params ()) (function_returns (Value (Type BoolType)))))
            (function_impl (Fn ((Block ((Break (Expr (Value (Bool true))))))))))))))))) |}]
