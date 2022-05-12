module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module E = Tact.Evaluator.Make (Syntax)
module Errors = Tact.Errors
module Zint = Tact.Zint
open Core

let make_errors () = new Errors.errors

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors ()) ?(eval_errors = make_errors ())
    ?(eval = true) ?(bindings = E.default_bindings) p =
  let c = new Lang.of_syntax_converter (bindings, errors) in
  let p' = c#visit_program () p in
  let c = new Lang.reference_resolver (p'.bindings, errors) in
  let p' = c#visit_program () p' in
  let p' =
    if eval then (new E.evaluator (p', eval_errors))#visit_program () p' else p'
  in
  errors#to_result p'
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> err) )

let print_sexp e =
  Sexplib.Sexp.pp_hum Format.std_formatter
    (Result.sexp_of_t Lang.sexp_of_program
       (List.sexp_of_t Lang.sexp_of_error)
       e )

let pp ?(eval = true) ?(bindings = E.default_bindings) s =
  parse_program s |> build_program ~eval ~bindings |> print_sexp

let%expect_test "scope resolution" =
  let source = {|
    let T = Int257;
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts ((Let ((T (Builtin Int257))))))
      (bindings
       ((T (Builtin Int257)) (Int257 (Builtin Int257)) (Bool (Builtin Bool))
        (Type (Builtin Type)) (Void Void))))) |}]

let%expect_test "binding resolution" =
  let source =
    {|
    let T = Int257;
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
       ((Let ((T (Builtin Int257)))) (Let ((T_ (Builtin Int257))))
        (Let ((a (Integer 1)))) (Let ((a_ (Integer 1))))))
      (bindings
       ((a_ (Integer 1)) (a (Integer 1)) (T_ (Builtin Int257))
        (T (Builtin Int257)) (Int257 (Builtin Int257)) (Bool (Builtin Bool))
        (Type (Builtin Type)) (Void Void))))) |}]

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     ((UnresolvedIdentifier Int256) (UnresolvedIdentifier Int256)
      (UnresolvedIdentifier Int256))) |}]

let%expect_test "scope resolution after let binding" =
  let source = {|
    let A = Int257;
    let B = A;
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts ((Let ((A (Builtin Int257)))) (Let ((B (Builtin Int257))))))
      (bindings
       ((B (Builtin Int257)) (A (Builtin Int257)) (Int257 (Builtin Int257))
        (Bool (Builtin Bool)) (Type (Builtin Type)) (Void Void))))) |}]

let%expect_test "basic struct definition" =
  let source = {|
    struct T { val t: Int257 }
  |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts
       ((Let
         ((T
           (Struct
            ((struct_fields ((t ((field_type (BuiltinType Int257))))))
             (struct_methods ()) (struct_id <opaque>))))))))
      (bindings
       ((T
         (Struct
          ((struct_fields ((t ((field_type (BuiltinType Int257))))))
           (struct_methods ()) (struct_id <opaque>))))
        (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
        (Void Void))))) |}]

let%expect_test "native function evaluation" =
  let source = {|
    incr(incr(incr(1)));
  |} in
  pp source
    ~bindings:
      ( ( "incr",
          Function
            (BuiltinFn
               { function_params = [("value", IntegerType)];
                 function_returns = IntegerType;
                 function_impl =
                   (fun _p -> function
                     | Integer arg :: _ ->
                         Integer (Zint.succ arg)
                     | _ ->
                         Integer Zint.zero ) } ) )
      :: E.default_bindings ) ;
  [%expect
    {|
    (Ok
     ((stmts ((Term (Integer 4))))
      (bindings
       ((incr
         (Function
          (BuiltinFn
           ((function_params ((value IntegerType)))
            (function_returns IntegerType) (function_impl <fun>)))))
        (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
        (Void Void))))) |}]

let%expect_test "Tact function evaluation" =
  let source =
    {|
    fn test(i: Int257) -> Int257 {
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
           (Function
            (Fn
             ((function_params ((i (BuiltinType Int257))))
              (function_returns (BuiltinType Int257))
              (function_impl (((Break (Term (Reference i))))))))))))
        (Let ((a (Integer 1))))))
      (bindings
       ((a (Integer 1))
        (test
         (Function
          (Fn
           ((function_params ((i (BuiltinType Int257))))
            (function_returns (BuiltinType Int257))
            (function_impl (((Break (Term (Reference i))))))))))
        (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
        (Void Void))))) |}]

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
       val a: Int257
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
             (Struct
              ((struct_fields
                ((a ((field_type (BuiltinType Int257))))
                 (b ((field_type (BuiltinType Bool))))))
               (struct_methods ()) (struct_id <opaque>))))))))
        (bindings
         ((MyType
           (Struct
            ((struct_fields
              ((a ((field_type (BuiltinType Int257))))
               (b ((field_type (BuiltinType Bool))))))
             (struct_methods ()) (struct_id <opaque>))))
          (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
          (Void Void))))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      val a: Int257
      val a: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
    (Error
     ((DuplicateField
       (a
        ((struct_fields
          ((a ((field_type (ReferenceType Int257))))
           (a ((field_type (ReferenceType Bool))))))
         (struct_methods ()) (struct_id <opaque>)))))) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T(A: Type) { val a: A }
      let TA = T(Int257);
   |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((stmts
       ((Let
         ((T
           (Function
            (Fn
             ((function_params ((A (BuiltinType Type))))
              (function_returns (BuiltinType Type))
              (function_impl
               (((Term
                  (Struct
                   ((struct_fields ((a ((field_type (ReferenceType A))))))
                    (struct_methods ()) (struct_id <opaque>)))))))))))))
        (Let
         ((TA
           (Struct
            ((struct_fields ((a ((field_type (BuiltinType Int257))))))
             (struct_methods ()) (struct_id <opaque>))))))))
      (bindings
       ((TA
         (Struct
          ((struct_fields ((a ((field_type (BuiltinType Int257))))))
           (struct_methods ()) (struct_id <opaque>))))
        (T
         (Function
          (Fn
           ((function_params ((A (BuiltinType Type))))
            (function_returns (BuiltinType Type))
            (function_impl
             (((Term
                (Struct
                 ((struct_fields ((a ((field_type (ReferenceType A))))))
                  (struct_methods ()) (struct_id <opaque>)))))))))))
        (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
        (Void Void))))) |}]

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
             (Function
              (Fn
               ((function_params ()) (function_returns HoleType)
                (function_impl (((Break (Term (Integer 1))))))))))))
          (Let ((a (Integer 1))))))
        (bindings
         ((a (Integer 1))
          (f
           (Function
            (Fn
             ((function_params ()) (function_returns HoleType)
              (function_impl (((Break (Term (Integer 1))))))))))
          (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
          (Void Void))))) |}]

let%expect_test "scoping that `let` introduces in code" =
  let source =
    {|
    fn f(i: Int257) {
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
           (Function
            (Fn
             ((function_params ((i (BuiltinType Int257))))
              (function_returns HoleType)
              (function_impl
               (((Let ((a (Reference i)))) (Break (Term (Reference a))))))))))))
        (Let ((b (Integer 1))))))
      (bindings
       ((b (Integer 1))
        (f
         (Function
          (Fn
           ((function_params ((i (BuiltinType Int257))))
            (function_returns HoleType)
            (function_impl
             (((Let ((a (Reference i)))) (Break (Term (Reference a))))))))))
        (Int257 (Builtin Int257)) (Bool (Builtin Bool)) (Type (Builtin Type))
        (Void Void)))))
     |}]
