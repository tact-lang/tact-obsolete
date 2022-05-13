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

let%expect_test "let struct" =
  let source = {|
    let MyType = struct {};
    |} in
  pp source ;
  [%expect
    {| ((stmts ((Let ((binding_name (Ident MyType)) (binding_expr (Struct ()))))))) |}]

let%expect_test "let struct with parameter (shorthand)" =
  let source = {|
  let MyType(T: Type) = struct {};
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident MyType))
         (binding_expr
          (Function
           ((params (((Ident T) (Reference (Ident Type)))))
            (returns (Reference (Ident Type)))
            (function_body ((function_stmts ((Expr (Struct ())))))))))))))) |}]

let%expect_test "struct definition (shorthand)" =
  let source = {|
  struct MyType {}
  |} in
  pp source ;
  [%expect
    {|
      ((stmts ((Let ((binding_name (Ident MyType)) (binding_expr (Struct ()))))))) |}]

let%expect_test "struct construction" =
  let source =
    {|
    struct MyType { 
     val a: Int(257)
     val b: Int(257)
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
      ((stmts
        ((Let
          ((binding_name (Ident MyType))
           (binding_expr
            (Struct
             ((fields
               (((field_name (Ident a))
                 (field_type
                  (FunctionCall
                   ((fn (Reference (Ident Int))) (arguments ((Int 257)))))))
                ((field_name (Ident b))
                 (field_type
                  (FunctionCall
                   ((fn (Reference (Ident Int))) (arguments ((Int 257))))))))))))))
         (Let
          ((binding_name (Ident my))
           (binding_expr
            (StructConstructor
             ((constructor_id (Reference (Ident MyType)))
              (fields_construction (((Ident a) (Int 0)) ((Ident b) (Int 1)))))))))))) |}]

let%expect_test "parameterized struct shorthand" =
  let source = {|
  struct MyType(T: Type) {}
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident MyType))
         (binding_expr
          (Function
           ((params (((Ident T) (Reference (Ident Type)))))
            (returns (Reference (Ident Type)))
            (function_body ((function_stmts ((Expr (Struct ())))))))))))))) |}]

let%expect_test "struct fields" =
  let source =
    {|
  struct MyType {
    val a: Int(257)
    val f: get_type()
  }
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident MyType))
         (binding_expr
          (Struct
           ((fields
             (((field_name (Ident a))
               (field_type
                (FunctionCall
                 ((fn (Reference (Ident Int))) (arguments ((Int 257)))))))
              ((field_name (Ident f))
               (field_type (FunctionCall ((fn (Reference (Ident get_type))))))))))))))))) |}]

let%expect_test "struct methods" =
  let source =
    {|
    struct MyType {
      fn test() -> Bool {}
      fn todo() -> Int(257)
    }
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident MyType))
         (binding_expr
          (Struct
           ((struct_bindings
             (((binding_name (Ident test))
               (binding_expr
                (Function
                 ((returns (Reference (Ident Bool))) (function_body ())))))
              ((binding_name (Ident todo))
               (binding_expr
                (Function
                 ((returns
                   (FunctionCall
                    ((fn (Reference (Ident Int))) (arguments ((Int 257)))))))))))))))))))) |}]

let%expect_test "struct with fields and methods" =
  let source =
    {|
    struct MyType {
      val a: Int(257)
      fn test() -> Bool {}
    }
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident MyType))
         (binding_expr
          (Struct
           ((fields
             (((field_name (Ident a))
               (field_type
                (FunctionCall
                 ((fn (Reference (Ident Int))) (arguments ((Int 257)))))))))
            (struct_bindings
             (((binding_name (Ident test))
               (binding_expr
                (Function
                 ((returns (Reference (Ident Bool))) (function_body ()))))))))))))))) |}]

let%expect_test "let function definition" =
  let source = {|
  let F = fn (A: T) -> P(1) {};
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (Function
           ((params (((Ident A) (Reference (Ident T)))))
            (returns
             (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1))))))
            (function_body ()))))))))) |}]

let%expect_test "function definition shorthand" =
  let source = {|
  fn F(A: T) -> P(1) {}
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (Function
           ((params (((Ident A) (Reference (Ident T)))))
            (returns
             (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1))))))
            (function_body ()))))))))) |}]

let%expect_test "function without a return type" =
  let source =
    {|
    let f1 = fn(t: Int(257));
    let f2 = fn(t: Int(257)) {};
    fn f4(t: Int(257)) {}
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident f1))
         (binding_expr
          (Function
           ((params
             (((Ident t)
               (FunctionCall
                ((fn (Reference (Ident Int))) (arguments ((Int 257)))))))))))))
       (Let
        ((binding_name (Ident f2))
         (binding_expr
          (Function
           ((params
             (((Ident t)
               (FunctionCall
                ((fn (Reference (Ident Int))) (arguments ((Int 257))))))))
            (function_body ()))))))
       (Let
        ((binding_name (Ident f4))
         (binding_expr
          (Function
           ((params
             (((Ident t)
               (FunctionCall
                ((fn (Reference (Ident Int))) (arguments ((Int 257))))))))
            (function_body ()))))))))) |}]

let%expect_test "parse parameterized return type as part of function \
                 signature, not a function call" =
  let source = {|
  let F = fn (A: T) -> P(1);
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (Function
           ((params (((Ident A) (Reference (Ident T)))))
            (returns
             (FunctionCall ((fn (Reference (Ident P))) (arguments ((Int 1)))))))))))))) |}]

let%expect_test "fn signature returning function signature" =
  let source = {|
  let F = fn (A: T) -> (fn () -> T);
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (Function
           ((params (((Ident A) (Reference (Ident T)))))
            (returns (Function ((returns (Reference (Ident T)))))))))))))) |}]

let%expect_test "enforcing precedence of a function call over a signature" =
  let source = {|
  let F = (fn (A: T) -> P)(1);
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (FunctionCall
           ((fn
             (Function
              ((params (((Ident A) (Reference (Ident T)))))
               (returns (Reference (Ident P))))))
            (arguments ((Int 1))))))))))) |}]

let%expect_test "function call" =
  let source = {|
  let F = func(1);
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (FunctionCall ((fn (Reference (Ident func))) (arguments ((Int 1))))))))))) |}]

let%expect_test "function call in a list of statements" =
  let source = {|
  let F = fn() -> T { 
       func(1);
  };
|} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident F))
         (binding_expr
          (Function
           ((returns (Reference (Ident T)))
            (function_body
             ((function_stmts
               ((Expr
                 (FunctionCall
                  ((fn (Reference (Ident func))) (arguments ((Int 1)))))))))))))))))) |}]

let%expect_test "let in function body" =
  let source =
    {|
  let f = fn() -> Int(257) { 
       let a = 1;
       return a;
  };
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident f))
         (binding_expr
          (Function
           ((returns
             (FunctionCall
              ((fn (Reference (Ident Int))) (arguments ((Int 257))))))
            (function_body
             ((function_stmts
               ((Let ((binding_name (Ident a)) (binding_expr (Int 1))))
                (Return (Reference (Ident a))))))))))))))) |}]

let%expect_test "code block without trailing semicolon" =
  let source =
    {|
    let f = fn() -> Int(257) { 
      let a = 1;
      a
    };
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident f))
         (binding_expr
          (Function
           ((returns
             (FunctionCall
              ((fn (Reference (Ident Int))) (arguments ((Int 257))))))
            (function_body
             ((function_stmts
               ((Let ((binding_name (Ident a)) (binding_expr (Int 1))))
                (Break (Expr (Reference (Ident a)))))))))))))))) |}]

let%expect_test "if with an empty body and no else statement" =
  let source = {|
  fn test() -> A {
    if (1) {}
  }
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body
             ((function_stmts ((Break (If ((condition (Int 1)) (body ())))))))))))))))) |}]

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
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body
             ((function_stmts
               ((Break
                 (If
                  ((condition (Int 1)) (body ((Expr (Reference (Ident a)))))
                   (else_ (CodeBlock ()))))))))))))))))) |}]

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
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body
             ((function_stmts
               ((Break
                 (If
                  ((condition (Int 1)) (body ())
                   (else_ (If ((condition (Int 10)) (body ()))))))))))))))))))) |}]

let%expect_test "struct construction over a parameterized type" =
  let source = {|
  let a = A(X) { field: value };
  |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident a))
         (binding_expr
          (StructConstructor
           ((constructor_id
             (FunctionCall
              ((fn (Reference (Ident A))) (arguments ((Reference (Ident X)))))))
            (fields_construction (((Ident field) (Reference (Ident value))))))))))))) |}]

let%expect_test "struct construction over an anonymous type" =
  let source =
    {|
  let a = (struct { val field: Int(257) }) { field: value };
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident a))
         (binding_expr
          (StructConstructor
           ((constructor_id
             (Struct
              ((fields
                (((field_name (Ident field))
                  (field_type
                   (FunctionCall
                    ((fn (Reference (Ident Int))) (arguments ((Int 257))))))))))))
            (fields_construction (((Ident field) (Reference (Ident value))))))))))))) |}]

let%expect_test "struct construction over an anonymous type's function call" =
  let source =
    {|
  let a = struct(T: Type) { val field: T }(X) { field: value };
  |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident a))
         (binding_expr
          (StructConstructor
           ((constructor_id
             (FunctionCall
              ((fn
                (Function
                 ((params (((Ident T) (Reference (Ident Type)))))
                  (returns (Reference (Ident Struct)))
                  (function_body
                   ((function_stmts
                     ((Expr
                       (Struct
                        ((fields
                          (((field_name (Ident field))
                            (field_type (Reference (Ident T))))))))))))))))
               (arguments ((Reference (Ident X)))))))
            (fields_construction (((Ident field) (Reference (Ident value))))))))))))) |}]

let%expect_test "tilde syntax" =
  let source = {|
    fn test() -> A {
      ~var;
    }
    |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body ((function_stmts ((Expr (MutRef (Ident var))))))))))))))) |}]

let%expect_test "field access syntax" =
  let source = {|
    fn test() -> A {
      foo.bar;
    }
    |} in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body
             ((function_stmts
               ((Expr
                 (FieldAccess
                  ((from_expr (Reference (Ident foo))) (to_field (Ident bar))))))))))))))))) |}]

let%expect_test "field access over other expressions" =
  let source =
    {|
    fn test() -> A {
      ~foo.bar;
      Struct{field: value}.field.other_field;
    }
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident test))
         (binding_expr
          (Function
           ((returns (Reference (Ident A)))
            (function_body
             ((function_stmts
               ((Expr
                 (FieldAccess
                  ((from_expr (MutRef (Ident foo))) (to_field (Ident bar)))))
                (Expr
                 (FieldAccess
                  ((from_expr
                    (FieldAccess
                     ((from_expr
                       (StructConstructor
                        ((constructor_id (Reference (Ident Struct)))
                         (fields_construction
                          (((Ident field) (Reference (Ident value))))))))
                      (to_field (Ident field)))))
                   (to_field (Ident other_field))))))))))))))))) |}]

let%expect_test "union definition" =
  let source =
    {|
    union U {
       case Int(257)
       case Bool
    }  
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident U))
         (binding_expr
          (Union
           ((union_members
             ((FunctionCall
               ((fn (Reference (Ident Int))) (arguments ((Int 257)))))
              (Reference (Ident Bool)))))))))))) |}]

let%expect_test "union definition using let binding" =
  let source =
    {|
    let U = union {
       case Int(257)
       case Bool
    };
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident U))
         (binding_expr
          (Union
           ((union_members
             ((FunctionCall
               ((fn (Reference (Ident Int))) (arguments ((Int 257)))))
              (Reference (Ident Bool)))))))))))) |}]

let%expect_test "parameterized union definition" =
  let source =
    {|
    union Option(T: Type) {
       case T
       case Null 
    }  
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident Option))
         (binding_expr
          (Function
           ((params (((Ident T) (Reference (Ident Type)))))
            (returns (Reference (Ident Type)))
            (function_body
             ((function_stmts
               ((Expr
                 (Union
                  ((union_members
                    ((Reference (Ident T)) (Reference (Ident Null))))))))))))))))))) |}]

let%expect_test "parameterized union definition using let binding" =
  let source =
    {|
    let Option(T: Type) = union {
       case T
       case Null 
    };
    |}
  in
  pp source ;
  [%expect
    {|
    ((stmts
      ((Let
        ((binding_name (Ident Option))
         (binding_expr
          (Function
           ((params (((Ident T) (Reference (Ident Type)))))
            (returns (Reference (Ident Type)))
            (function_body
             ((function_stmts
               ((Expr
                 (Union
                  ((union_members
                    ((Reference (Ident T)) (Reference (Ident Null))))))))))))))))))) |}]
