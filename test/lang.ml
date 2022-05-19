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
     ((stmts
       ((Let
         ((T
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>)))))))))
      (bindings
       ((T
         (Value
          (Struct
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>)))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

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
     ((stmts
       ((Let
         ((T
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>)))))))
        (Let
         ((T_
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>)))))))
        (Let ((a (Value (Integer 1))))) (Let ((a_ (Value (Integer 1)))))))
      (bindings
       ((a_ (Value (Integer 1))) (a (Value (Integer 1)))
        (T_
         (Value
          (Struct
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>)))))
        (T
         (Value
          (Struct
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>)))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (((UnresolvedIdentifier Int256)
       ((stmts ((Let ((T (Reference (Int256 HoleType)))))))
        (bindings
         ((asm
           (Value
            (Function
             ((function_signature
               ((function_params ((instructions (Value (Type StringType)))))
                (function_returns (Value (Type VoidType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits (Value (Type IntegerType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Builtin Bool))) (Type (Value (Type TypeType)))
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
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (new_builder
           (Value
            (Function
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (AsmFn NEWC))))))
          (store_uint
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((value (Value (Type IntegerType)))
                  (builder (Value (Type (BuiltinType Builder))))
                  (bits (Value (Type IntegerType)))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (AsmFn STIX))))))
          (end_builder
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Cell))))))
              (function_impl (AsmFn ENDC)))))))))))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts
       ((Let
         ((A
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>)))))))
        (Let
         ((B
           (Value
            (Struct
             ((struct_fields
               ((integer ((field_type (Value (Type IntegerType)))))))
              (struct_id <opaque>)))))))))
      (bindings
       ((B
         (Value
          (Struct
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>)))))
        (A
         (Value
          (Struct
           ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
            (struct_id <opaque>)))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts
       ((Let
         ((T
           (Value
            (Struct
             ((struct_fields
               ((t
                 ((field_type
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))))
              (struct_id <opaque>)))))))))
      (bindings
       ((T
         (Value
          (Struct
           ((struct_fields
             ((t
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))))
            (struct_id <opaque>)))))))
      (methods
       (((Struct
          ((struct_fields
            ((t
              ((field_type
                (Value
                 (Struct
                  ((struct_fields
                    ((integer ((field_type (Value (Type IntegerType)))))))
                   (struct_id <opaque>)))))))))
           (struct_id <opaque>)))
         ())
        ((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
  [%expect
    {|
    (Ok
     ((stmts ((Let ((v (Value (Integer 4)))))))
      (bindings ((v (Value (Integer 4))))))) |}]

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
     ((stmts
       ((Let
         ((test
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((i
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (function_returns
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
        (Let ((a (Value (Integer 1)))))))
      (bindings
       ((a (Value (Integer 1)))
        (test
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

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
     ((stmts
       ((Let
         ((test
           (Value
            (Function
             ((function_signature ((function_params ()) (function_returns Hole)))
              (function_impl
               (Fn
                (((Let ((v (Value (Integer 4)))))
                  (Break (Expr (Value (Integer 4))))))))))))))))
      (bindings
       ((test
         (Value
          (Function
           ((function_signature ((function_params ()) (function_returns Hole)))
            (function_impl
             (Fn
              (((Let ((v (Value (Integer 4)))))
                (Break (Expr (Value (Integer 4)))))))))))))))) |}]

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
       ((stmts
         ((Let
           ((MyType
             (Value
              (Struct
               ((struct_fields
                 ((a
                   ((field_type
                     (Value
                      (Struct
                       ((struct_fields
                         ((integer ((field_type (Value (Type IntegerType)))))))
                        (struct_id <opaque>)))))))
                  (b ((field_type (Value (Builtin Bool)))))))
                (struct_id <opaque>)))))))))
        (bindings
         ((MyType
           (Value
            (Struct
             ((struct_fields
               ((a
                 ((field_type
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (b ((field_type (Value (Builtin Bool)))))))
              (struct_id <opaque>)))))))
        (methods
         (((Struct
            ((struct_fields
              ((a
                ((field_type
                  (Value
                   (Struct
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>)))))))
               (b ((field_type (Value (Builtin Bool)))))))
             (struct_id <opaque>)))
           ())
          ((Struct
            ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((builder (Value (Type (BuiltinType Builder))))
                  (value (Value (Type HoleType)))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                (((Return
                   (FunctionCall
                    ((Reference (store_uint HoleType))
                     ((StructField ((Reference (value HoleType)) integer))
                      (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

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
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (a ((field_type (Value (Builtin Bool)))))))
          (struct_id <opaque>))))
       ((stmts
         ((Let
           ((MyType
             (Value
              (Struct
               ((struct_fields
                 ((a
                   ((field_type
                     (Value
                      (Struct
                       ((struct_fields
                         ((integer ((field_type (Value (Type IntegerType)))))))
                        (struct_id <opaque>)))))))
                  (a ((field_type (Value (Builtin Bool)))))))
                (struct_id <opaque>)))))))))
        (bindings
         ((MyType
           (Value
            (Struct
             ((struct_fields
               ((a
                 ((field_type
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (a ((field_type (Value (Builtin Bool)))))))
              (struct_id <opaque>)))))
          (asm
           (Value
            (Function
             ((function_signature
               ((function_params ((instructions (Value (Type StringType)))))
                (function_returns (Value (Type VoidType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Builder (Value (Type (BuiltinType Builder))))
          (Integer (Value (Type IntegerType)))
          (Int
           (Value
            (Function
             ((function_signature
               ((function_params ((bits (Value (Type IntegerType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (Bool (Value (Builtin Bool))) (Type (Value (Type TypeType)))
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
              (function_impl (BuiltinFn (<fun> <opaque>)))))))
          (new_builder
           (Value
            (Function
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (AsmFn NEWC))))))
          (store_uint
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((value (Value (Type IntegerType)))
                  (builder (Value (Type (BuiltinType Builder))))
                  (bits (Value (Type IntegerType)))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (AsmFn STIX))))))
          (end_builder
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((builder (Value (Type (BuiltinType Builder))))))
                (function_returns (Value (Type (BuiltinType Cell))))))
              (function_impl (AsmFn ENDC))))))))
        (methods
         (((Struct
            ((struct_fields
              ((a
                ((field_type
                  (Value
                   (Struct
                    ((struct_fields
                      ((integer ((field_type (Value (Type IntegerType)))))))
                     (struct_id <opaque>)))))))
               (a ((field_type (Value (Builtin Bool)))))))
             (struct_id <opaque>)))
           ())
          ((Struct
            ((struct_fields
              ((integer ((field_type (Value (Type IntegerType)))))))
             (struct_id <opaque>)))
           ((new
             ((function_signature
               ((function_params ((integer (Value (Type IntegerType)))))
                (function_returns
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_impl (BuiltinFn (<fun> <opaque>)))))
            (serialize
             ((function_signature
               ((function_params
                 ((builder (Value (Type (BuiltinType Builder))))
                  (value (Value (Type HoleType)))))
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl
               (Fn
                (((Return
                   (FunctionCall
                    ((Reference (store_uint HoleType))
                     ((StructField ((Reference (value HoleType)) integer))
                      (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))))) |}]

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
     ((stmts
       ((Let
         ((T
           (Value
            (Function
             ((function_signature
               ((function_params ((A (Value (Type TypeType)))))
                (function_returns (Value (Type TypeType)))))
              (function_impl
               (Fn
                (((Expr
                   (Value
                    (Struct
                     ((struct_fields
                       ((a ((field_type (Reference (A TypeType)))))))
                      (struct_id <opaque>)))))))))))))))
        (Let
         ((TA
           (Value
            (Struct
             ((struct_fields
               ((a
                 ((field_type
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))))
              (struct_id <opaque>)))))))))
      (bindings
       ((TA
         (Value
          (Struct
           ((struct_fields
             ((a
               ((field_type
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))))
            (struct_id <opaque>)))))
        (T
         (Value
          (Function
           ((function_signature
             ((function_params ((A (Value (Type TypeType)))))
              (function_returns (Value (Type TypeType)))))
            (function_impl
             (Fn
              (((Expr
                 (Value
                  (Struct
                   ((struct_fields ((a ((field_type (Reference (A TypeType)))))))
                    (struct_id <opaque>)))))))))))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257))))))))))))))
        ((Struct
          ((struct_fields ((a ((field_type (Reference (A TypeType)))))))
           (struct_id <opaque>)))
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
       ((stmts
         ((Let
           ((f
             (Value
              (Function
               ((function_signature ((function_params ()) (function_returns Hole)))
                (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))))
          (Let ((a (Value (Integer 1)))))))
        (bindings
         ((a (Value (Integer 1)))
          (f
           (Value
            (Function
             ((function_signature ((function_params ()) (function_returns Hole)))
              (function_impl (Fn (((Break (Expr (Value (Integer 1)))))))))))))))) |}]

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
     ((stmts
       ((Let
         ((f
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((i
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (function_returns Hole)))
              (function_impl
               (Fn
                (((Let ((a (Reference (i TypeType)))))
                  (Break (Expr (Reference (a TypeType))))))))))))))
        (Let ((b (Value (Integer 1)))))))
      (bindings
       ((b (Value (Integer 1)))
        (f
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_returns Hole)))
            (function_impl
             (Fn
              (((Let ((a (Reference (i TypeType)))))
                (Break (Expr (Reference (a TypeType))))))))))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257))))))))))))))))))
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
     ((stmts
       ((Let
         ((op
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((i
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))
                  (i_
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (function_returns Hole)))
              (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
        (Let
         ((f
           (Value
            (Function
             ((function_signature
               ((function_params
                 ((x
                   (Value
                    (Struct
                     ((struct_fields
                       ((integer ((field_type (Value (Type IntegerType)))))))
                      (struct_id <opaque>)))))))
                (function_returns Hole)))
              (function_impl
               (Fn
                (((Let
                   ((a
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((i
                               (Value
                                (Struct
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>)))))
                              (i_
                               (Value
                                (Struct
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>)))))))
                            (function_returns Hole)))
                          (function_impl
                           (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                       ((Reference (x TypeType)) (Reference (x TypeType))))))))
                  (Let
                   ((b
                     (FunctionCall
                      ((Value
                        (Function
                         ((function_signature
                           ((function_params
                             ((i
                               (Value
                                (Struct
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>)))))
                              (i_
                               (Value
                                (Struct
                                 ((struct_fields
                                   ((integer
                                     ((field_type (Value (Type IntegerType)))))))
                                  (struct_id <opaque>)))))))
                            (function_returns Hole)))
                          (function_impl
                           (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                       ((Reference (a HoleType)) (Reference (a HoleType))))))))))))))))))))
      (bindings
       ((f
         (Value
          (Function
           ((function_signature
             ((function_params
               ((x
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_returns Hole)))
            (function_impl
             (Fn
              (((Let
                 ((a
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params
                           ((i
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))
                            (i_
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))))
                          (function_returns Hole)))
                        (function_impl
                         (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                     ((Reference (x TypeType)) (Reference (x TypeType))))))))
                (Let
                 ((b
                   (FunctionCall
                    ((Value
                      (Function
                       ((function_signature
                         ((function_params
                           ((i
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))
                            (i_
                             (Value
                              (Struct
                               ((struct_fields
                                 ((integer
                                   ((field_type (Value (Type IntegerType)))))))
                                (struct_id <opaque>)))))))
                          (function_returns Hole)))
                        (function_impl
                         (Fn (((Break (Expr (Reference (i TypeType)))))))))))
                     ((Reference (a HoleType)) (Reference (a HoleType))))))))))))))))
        (op
         (Value
          (Function
           ((function_signature
             ((function_params
               ((i
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))
                (i_
                 (Value
                  (Struct
                   ((struct_fields
                     ((integer ((field_type (Value (Type IntegerType)))))))
                    (struct_id <opaque>)))))))
              (function_returns Hole)))
            (function_impl (Fn (((Break (Expr (Reference (i TypeType))))))))))))))
      (methods
       (((Struct
          ((struct_fields ((integer ((field_type (Value (Type IntegerType)))))))
           (struct_id <opaque>)))
         ((new
           ((function_signature
             ((function_params ((integer (Value (Type IntegerType)))))
              (function_returns
               (Value
                (Struct
                 ((struct_fields
                   ((integer ((field_type (Value (Type IntegerType)))))))
                  (struct_id <opaque>)))))))
            (function_impl (BuiltinFn (<fun> <opaque>)))))
          (serialize
           ((function_signature
             ((function_params
               ((builder (Value (Type (BuiltinType Builder))))
                (value (Value (Type HoleType)))))
              (function_returns (Value (Type (BuiltinType Builder))))))
            (function_impl
             (Fn
              (((Return
                 (FunctionCall
                  ((Reference (store_uint HoleType))
                   ((StructField ((Reference (value HoleType)) integer))
                    (Reference (builder HoleType)) (Value (Integer 257)))))))))))))))))) |}]

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
     ((stmts
       ((Let ((i (Value (Integer 1)))))
        (Let
         ((f
           (Value
            (Function
             ((function_signature ((function_params ()) (function_returns Hole)))
              (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))))
        (Let ((x (Value (Integer 1)))))))
      (bindings
       ((x (Value (Integer 1)))
        (f
         (Value
          (Function
           ((function_signature ((function_params ()) (function_returns Hole)))
            (function_impl (Fn (((Break (Expr (Value (Integer 1))))))))))))
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
     ((stmts
       ((Let ((Foo (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))
        (Let
         ((foo
           (Value
            (StructInstance (((struct_fields ()) (struct_id <opaque>)) ()))))))
        (Let ((res (Value (Integer 1)))))))
      (bindings
       ((res (Value (Integer 1)))
        (foo
         (Value (StructInstance (((struct_fields ()) (struct_id <opaque>)) ()))))
        (Foo (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))
      (methods
       (((Struct ((struct_fields ()) (struct_id <opaque>)))
         ((bar
           ((function_signature
             ((function_params
               ((self (Value (Type TypeType))) (i (Value (Type IntegerType)))))
              (function_returns Hole)))
            (function_impl (Fn (((Break (Expr (Reference (i IntegerType)))))))))))))))) |}]

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
     ((stmts
       ((Let ((Foo (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))))
      (bindings
       ((Foo (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))
      (methods
       (((Struct ((struct_fields ()) (struct_id <opaque>)))
         ((bar
           ((function_signature
             ((function_params
               ((self (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))
              (function_returns
               (Value (Struct ((struct_fields ()) (struct_id <opaque>)))))))
            (function_impl (Fn (((Break (Expr (Reference (self TypeType)))))))))))))))) |}]

let%expect_test "can't use asm in compile-time" =
  let source = {|
    asm("SWAP 2 XCHG0");
    asm("ADD");
  |} in
  pp source ;
  [%expect
    {|
    (Ok ((stmts ((Expr (Value Void)) (Expr (Value Void)))) (bindings ()))) |}]

let%expect_test "use of asm in a function" =
  let source =
    {|
    fn f() {
      asm("SWAP 2 XCHG0");
      asm("ADD");
    }
  |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts
       ((Let
         ((f
           (Value
            (Function
             ((function_signature ((function_params ()) (function_returns Hole)))
              (function_impl (Fn (((Expr (Value Void)) (Expr (Value Void))))))))))))))
      (bindings
       ((f
         (Value
          (Function
           ((function_signature ((function_params ()) (function_returns Hole)))
            (function_impl (Fn (((Expr (Value Void)) (Expr (Value Void)))))))))))))) |}]
