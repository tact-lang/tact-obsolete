open Core
module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let print_sexp p =
  Sexplib.Sexp.pp_hum Format.std_formatter (Syntax.sexp_of_program p)

let pp s = parse_program s |> print_sexp

let%expect_test "empty" =
  let source = {||} in
  pp source ; [%expect {| () |}]

let%expect_test "let type" =
  let source = {|
    let MyType = type {};
    |} in
  pp source ;
  [%expect
    {| ((bindings (((binding_name (Ident MyType)) (binding_expr (Type ())))))) |}]

let%expect_test "let type with parameter (shorthand)" =
  let source = {|
  let MyType(T: Type) = type {};
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Function
          ((params (((Ident T) (Reference (Ident Type)))))
           (returns (Reference (Ident Type))) (exprs ((Type ())))))))))) |}]

let%expect_test "type definition (shorthand)" =
  let source = {|
  type MyType {}
  |} in
  pp source ;
  [%expect
    {| ((bindings (((binding_name (Ident MyType)) (binding_expr (Type ())))))) |}]

let%expect_test "type construction" =
  let source =
    {|
  type MyType { 
     a: Int257,
     b: Int257
  }
  let my = MyType {
    a: 0,
    b: 1
  };
  |}
  in
  pp source ;
  [%expect
    {|
      ((bindings
        (((binding_name (Ident MyType))
          (binding_expr
           (Type
            ((fields
              (((field_name (Ident a)) (field_type (Reference (Ident Int257))))
               ((field_name (Ident b)) (field_type (Reference (Ident Int257))))))))))
         ((binding_name (Ident my))
          (binding_expr
           (TypeConstructor
            ((constructor_id (Reference (Ident MyType)))
             (fields_construction (((Ident a) (Int 0)) ((Ident b) (Int 1))))))))))) |}]

let%expect_test "parameterized type shorthand" =
  let source = {|
  type MyType(T: Type) {}
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Function
          ((params (((Ident T) (Reference (Ident Type)))))
           (returns (Reference (Ident Type))) (exprs ((Type ())))))))))) |}]

let%expect_test "type fields" =
  let source = {|
  type MyType {
    a: Int257,
    f: get_type()
  }
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((fields
            (((field_name (Ident a)) (field_type (Reference (Ident Int257))))
             ((field_name (Ident f))
              (field_type (FunctionCall ((fn (Reference (Ident get_type)))))))))))))))) |}]

let%expect_test "type fields with a trailing comma" =
  let source = {|
  type MyType {
    a: Int257,
    f: get_type(),
  }
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((fields
            (((field_name (Ident a)) (field_type (Reference (Ident Int257))))
             ((field_name (Ident f))
              (field_type (FunctionCall ((fn (Reference (Ident get_type)))))))))))))))) |}]

let%expect_test "type fields with shorthand names" =
  let source = {|
    type MyType {
      A,
      B
    }
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((fields
            (((field_name (Ident A)) (field_type (Reference (Ident A))))
             ((field_name (Ident B)) (field_type (Reference (Ident B))))))))))))) |}]

let%expect_test "type methods" =
  let source =
    {|
    type MyType {
      fn test() -> Bool {}
      fn todo() -> Int257
    }
  |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((type_bindings
            (((binding_name (Ident test))
              (binding_expr
               (Function ((returns (Reference (Ident Bool))) (exprs ())))))
             ((binding_name (Ident todo))
              (binding_expr (Function ((returns (Reference (Ident Int257)))))))))))))))) |}]

let%expect_test "type with fields and methods" =
  let source =
    {|
    type MyType {
      a: Int257
      fn test() -> Bool {}
    }
  |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((fields
            (((field_name (Ident a)) (field_type (Reference (Ident Int257))))))
           (type_bindings
            (((binding_name (Ident test))
              (binding_expr
               (Function ((returns (Reference (Ident Bool))) (exprs ())))))))))))))) |}]

let%expect_test "type with fields and methods, separated by a comma" =
  let source =
    {|
    type MyType {
      a: Int257,
      fn test() -> Bool {}
    }
  |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident MyType))
        (binding_expr
         (Type
          ((fields
            (((field_name (Ident a)) (field_type (Reference (Ident Int257))))))
           (type_bindings
            (((binding_name (Ident test))
              (binding_expr
               (Function ((returns (Reference (Ident Bool))) (exprs ())))))))))))))) |}]

let%expect_test "let function definition" =
  let source = {|
  let F = fn (A: T) -> P(1) {};
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (Function
          ((params (((Ident A) (Reference (Ident T)))))
           (returns
            (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1))))))
           (exprs ())))))))) |}]

let%expect_test "function definition shorthand" =
  let source = {|
  fn F(A: T) -> P(1) {}
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (Function
          ((params (((Ident A) (Reference (Ident T)))))
           (returns
            (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1))))))
           (exprs ())))))))) |}]

let%expect_test "parse parameterized return type as part of function \
                 signature, not a function call" =
  let source = {|
  let F = fn (A: T) -> P(1);
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (Function
          ((params (((Ident A) (Reference (Ident T)))))
           (returns
            (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1))))))))))))) |}]

let%expect_test "fn signature returning function signature" =
  let source = {|
  let F = fn (A: T) -> (fn () -> T);
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (Function
          ((params (((Ident A) (Reference (Ident T)))))
           (returns (Function ((returns (Reference (Ident T))))))))))))) |}]

let%expect_test "enforcing precedence of a function call over a signature" =
  let source = {|
  let F = (fn (A: T) -> P)(1);
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (FunctionCall
          ((fn
            (Function
             ((params (((Ident A) (Reference (Ident T)))))
              (returns (Reference (Ident P))))))
           (arguments ((Int 1)))))))))) |}]

let%expect_test "function call" =
  let source = {|
  let F = func(1);
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (FunctionCall ((fn (Reference (Ident func))) (arguments ((Int 1)))))))))) |}]

let%expect_test "function call in a list of statements" =
  let source = {|
  let F = fn() -> T { 
       func(1);
  };
|} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident F))
        (binding_expr
         (Function
          ((returns (Reference (Ident T)))
           (exprs
            ((FunctionCall ((fn (Reference (Ident func))) (arguments ((Int 1)))))))))))))) |}]

let%expect_test "let in function body" =
  let source =
    {|
  let f = fn() -> Int257 { 
       let a = 1;
       return a;
  };
  |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident f))
        (binding_expr
         (Function
          ((returns (Reference (Ident Int257)))
           (exprs
            ((Let ((binding_name (Ident a)) (binding_expr (Int 1))))
             (Return (Reference (Ident a)))))))))))) |}]

let%expect_test "if with an empty body and no else statement" =
  let source = {|
  fn test() -> A {
    if (1) {}
  }
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident test))
        (binding_expr
         (Function
          ((returns (Reference (Ident A))) (exprs ((If ((condition (Int 1))))))))))))) |}]

let%expect_test "if with a body and an empty else" =
  let source =
    {|
          fn test() -> A {
            if (1) {
              a;
            }
            else {}
          }
          |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident test))
        (binding_expr
         (Function
          ((returns (Reference (Ident A)))
           (exprs
            ((If
              ((condition (Int 1)) (body ((Reference (Ident a))))
               (else_ (CodeBlock ()))))))))))))) |}]

let%expect_test "if with else if" =
  let source =
    {|
    fn test() -> A {
      if (1) {}
      else if (10) {}
    }
    |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident test))
        (binding_expr
         (Function
          ((returns (Reference (Ident A)))
           (exprs
            ((If ((condition (Int 1)) (else_ (If ((condition (Int 10)))))))))))))))) |}]

let%expect_test "type construction over a parameterized type" =
  let source = {|
  let a = A(X) { field: value };
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident a))
        (binding_expr
         (TypeConstructor
          ((constructor_id
            (FunctionCall
             ((fn (Reference (Ident A))) (arguments ((Reference (Ident X)))))))
           (fields_construction (((Ident field) (Reference (Ident value)))))))))))) |}]

let%expect_test "type construction over an anonymous type" =
  let source = {|
  let a = (type { field: Int257 }) { field: value };
  |} in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident a))
        (binding_expr
         (TypeConstructor
          ((constructor_id
            (Type
             ((fields
               (((field_name (Ident field))
                 (field_type (Reference (Ident Int257)))))))))
           (fields_construction (((Ident field) (Reference (Ident value)))))))))))) |}]

let%expect_test "type construction over an anonymous type's function call" =
  let source =
    {|
  let a = type(T: Type) { field: T }(X) { field: value };
  |}
  in
  pp source ;
  [%expect
    {|
    ((bindings
      (((binding_name (Ident a))
        (binding_expr
         (TypeConstructor
          ((constructor_id
            (FunctionCall
             ((fn
               (Function
                ((params (((Ident T) (Reference (Ident Type)))))
                 (returns (Reference (Ident Type)))
                 (exprs
                  ((Type
                    ((fields
                      (((field_name (Ident field))
                        (field_type (Reference (Ident T)))))))))))))
              (arguments ((Reference (Ident X)))))))
           (fields_construction (((Ident field) (Reference (Ident value)))))))))))) |}]
