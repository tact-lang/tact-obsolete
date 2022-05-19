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
    ◉
    └─T│struct│┌───────┬───┐
       │      ││integer│int│
       │      │└───────┴───┘ |}]

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
    ◉
    ├─a_│1
    ├─a││
    ├─T_│struct│┌───────┬───┐
    │   │      ││integer│int│
    │   │      │└───────┴───┘
    └─T│struct│├───────┬───┐
       │      ││integer│int│
       │      │└───────┴───┘ |}]

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
         ((Builder (Value (Type (BuiltinType Builder))))
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
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
        (methods
         (((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn (((Return (Primitive EmptyBuilder)))))))))))))))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int(257);
    let B = A;
  |} in
  pp source ;
  [%expect
    {|
    ◉
    ├─B│struct│┌───────┬───┐
    │  │      ││integer│int│
    │  │      │└───────┴───┘
    └─A│struct│┌───────┬───┐
       │      ││integer│int│
       │      │└───────┴───┘ |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int(257) }
  |} in
  pp source ;
  [%expect
    {|
    ◉
    └─T│struct│┌─┬──────┬─────────────┐
       │      ││t│struct│┌───────┬───┐│
       │      ││ │      ││integer│int││
       │      ││ │      │└───────┴───┘│
       │      │└─┴──────┴─────────────┘ |}]

let%expect_test "native function evaluation" =
  let source = {|
    let v = incr(incr(incr(1)));
  |} in
  pp source ~bindings:(("incr", Value incr_f) :: Lang.default_bindings) ;
  [%expect {|
    ◉
    └─v│4 |}]

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
    ◉
    ├─a│1
    └─test│fn │i                   │ ⟶ │struct│┌───────┬───┐
          │   ├──────┬─────────────┤   │      ││integer│int│
          │   │struct│┌───────┬───┐│   │      │└───────┼───┘
          │   │      ││integer│int││   │      │        │
          │   │      │└───────┴───┘│   │      │
          ├───┴─┬─┬──┴┬────────────┴───┴──────┴─────────────
          │break│i│ : │Type |}]

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
    ◉
    └─test│fn ├┤ ⟶ │_
          ├───┴┴───┴────
          │┌───────────┐
          ││Let binding│
          │├─┬─────────┤
          ││v│4        │
          │└─┴─────────┘
          ├─────┬─┬─────
          │break│v│↗ |}]

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
      ◉
      └─MyType│struct│┌─┬──────┬─────────────┐
              │      ││a│struct│┌───────┬───┐│
              │      ││ │      ││integer│int││
              │      ││ │      │└───────┴───┘│
              │      │├─┼──────┴─────────────┤
              │      ││b│(Bool)              │
              │      │└─┴────────────────────┘ |}]

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
              (function_impl (BuiltinFn (<fun> <opaque>)))))))))
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
                (((Return
                   (Primitive
                    (StoreInt
                     (builder (Reference (builder (BuiltinType Builder))))
                     (length (Value (Integer 257)))
                     (integer
                      (StructField
                       ((Reference
                         (self
                          (StructType
                           ((struct_fields
                             ((integer ((field_type (Value (Type IntegerType)))))))
                            (struct_id <opaque>)))))
                        integer)))
                     (signed true))))))))))))
          ((Type (BuiltinType Builder))
           ((new
             ((function_signature
               ((function_params ())
                (function_returns (Value (Type (BuiltinType Builder))))))
              (function_impl (Fn (((Return (Primitive EmptyBuilder)))))))))))))))) |}]

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
    ◉
    ├─TA│struct│┌─┬──────┬─────────────┐
    │   │      ││a│struct│┌───────┬───┐│
    │   │      ││ │      ││integer│int││
    │   │      ││ │      │└───────┴───┘│
    │   │      │├─┴─┬────┴─────────────┘
    └─T│fn │A   │ ⟶ │Type
       │   ├────┤   │
       │   │Type│   │
       ├───┴──┬─┴───┴────────
       │struct│┌─┬─┬───┬────┐
       │      ││a│A│ : │Type│
       │      │└─┴─┴───┴────┘ |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp source ;
  [%expect
    {|
      ◉
      ├─a│1
      └─f│fn ├┤ ⟶ │_
         ├───┴┴┬──┴─
         │break│1 |}]

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
    ◉
    ├─b│1
    └─f│fn │i                   │ ⟶ │_
       │   ├──────┬─────────────┤   │
       │   │struct│┌───────┬───┐│   │
       │   │      ││integer│int││   │
       │   │      │└───────┴───┘│   │
       ├───┴──────┴─────────────┴───┴─
       │┌────────────┐
       ││Let binding │
       │├─┬─┬───┬────┼───────────────
       ││a│i│ : │Type│
       │└─┴─┴───┴────┘
       ├─────┬─┬───┬──────────────────
       │break│a│ : │Type
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
    ◉
    ├─f│fn │x                   │ ⟶ │_
    │  │   ├──────┬─────────────┤   │
    │  │   │struct│┌───────┬───┐│   │
    │  │   │      ││integer│int││   │
    │  │   │      │└───────┴───┘│   │
    │  ├───┴──────┴─────────────┴───┴───────
    │  │┌──────────────────────────────────┐
    │  ││Let binding                       │
    │  │├─┬────────────────────────────────┤
    │  ││a│┌──┬─┬─┬─┬───┬────┬─┬───┬────┬─┐│
    │  ││ ││op│↗│(│x│ : │Type│x│ : │Type│)││
    │  ││ │└──┴─┴─┴─┴───┴────┴─┴───┴────┴─┘│
    │  │└─┴────────────────────────────────┘
    │  ├────────────────────────────────────
    │  │┌────────────────────────────┐
    │  ││Let binding                 │
    │  │├─┬──────────────────────────┼─────
    │  ││b│┌──┬─┬─┬─┬───┬─┬─┬───┬─┬─┐│
    │  ││ ││op│↗│(│a│ : │_│a│ : │_│)││
    │  ││ │└──┴─┴─┴─┴───┴─┴─┴───┴─┴─┘│
    │  │├─┴─┬────────────────────┬───┘
    └─op│fn │i                   │i_                  │ ⟶ │_
        │   ├──────┬─────────────┼──────┬─────────────┤   │
        │   │struct│┌───────┬───┐│struct│┌───────┬───┐│   │
        │   │      ││integer│int││      ││integer│int││   │
        │   │      │└───────┴───┘│      │└───────┴───┘│   │
        ├───┴─┬─┬──┴┬────────────┴──────┴─────────────┴───┴─
        │break│i│ : │Type |}]

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
    ◉
    ├─x│1
    ├─f│fn ├┤ ⟶ │_
    │  ├───┴┴┬─┬┴─
    │  │break│i│↗
    └─i│1 |}]

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
  [%expect {|
    ◉
    ├─res│1
    ├─foo│struct instance
    └─Foo│struct│┼ |}]

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
  pp source ; [%expect {|
    ◉
    └─Foo│struct│┼ |}]
