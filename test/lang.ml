module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
open Lang
open Core

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program stx =
  let elist = make_error_list ~warnings:(ref []) ~errors:(ref []) () in
  let env = env_from_program stx elist in
  match (!(elist.errors), !(elist.warnings)) with
  | [], _ ->
      Ok env
  | errors :: _, _ ->
      Error errors

let print_sexp e =
  Sexplib.Sexp.pp_hum Format.std_formatter
    (Result.sexp_of_t Lang.sexp_of_env Lang.sexp_of_error e)

let pp s = parse_program s |> build_program |> print_sexp

let pp_stripped s =
  Result.map
    (parse_program s |> build_program)
    ~f:(fun env -> (new resolved_references_stripper env)#visit_env () env)
  |> print_sexp

let%expect_test "scope resolution" =
  let source =
    {|
  let I = Int257;
  let I_ = I;
  let n = 1;
  let n_ = n;
  |}
  in
  pp source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (I (ResolvedReference Int257 (Builtin Int257)))
        (I_ (ResolvedReference I (Builtin Int257))) (Int257 (Builtin Int257))
        (Type (Builtin Type)) (Void Void) (n (Integer 1))
        (n_ (ResolvedReference n (Integer 1)))
        (println (Function (BuiltinFn <fun>))))))) |}]

let%expect_test "stripping scope resolution" =
  let source =
    {|
  let I = Int257;
  let I_ = I;
  let n = 1;
  let n_ = n;
  |}
  in
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (I (Builtin Int257)) (I_ (Builtin Int257))
        (Int257 (Builtin Int257)) (Type (Builtin Type)) (Void Void)
        (n (Integer 1)) (n_ (Integer 1)) (println (Function (BuiltinFn <fun>))))))) |}]

let%expect_test "scope resolution within functions" =
  let source =
    {|
    let i = Int257;
    fn test(i: Int257) -> Void {
      i;
    }
  |}
  in
  pp source ;
  (* The idea here is that `i` inside of `test` won't resolve to the top-level `i` *)
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void) (i (ResolvedReference Int257 (Builtin Int257)))
        (println (Function (BuiltinFn <fun>)))
        (test
         (Function
          (Fn
           ((function_params
             ((i (ResolvedReferenceKind Int257 (BuiltinKind Int257)))))
            (function_returns (ResolvedReferenceKind Void VoidKind))
            (function_body ((Term (Reference i)))))))))))) |}]

let%expect_test "recursive scope resolution" =
  let source = {|
  let A = B;
  let B = C;
  let C = A;
  |} in
  pp source ; [%expect {| (Error (Recursive_Reference C)) |}]

let%expect_test "type definition" =
  let source =
    {|
  let MyType = type {
       a: Int257,
       b: Bool
  };
  |}
  in
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257))
        (MyType
         (Type
          ((type_fields
            ((a ((field_type (BuiltinKind Int257))))
             (b ((field_type (BuiltinKind Bool))))))
           (type_methods ()))))
        (Type (Builtin Type)) (Void Void) (println (Function (BuiltinFn <fun>))))))) |}]

let%expect_test "duplicate type" =
  let source = {|
  let MyType = type {};
  let MyType = type {};
  |} in
  pp source ;
  [%expect
    {|
    (Error
     (Duplicate_Identifier MyType (Type ((type_fields ()) (type_methods ()))))) |}]

let%expect_test "duplicate but of a different kind" =
  let source = {|
  let MyType = 1;
  let MyType = type {};
  |} in
  pp source ;
  [%expect {|
    (Error (Duplicate_Identifier MyType (Integer 1))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = type {
      a: Int257,
      a: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
    (Error
     (Duplicate_Field a
      ((type_fields ((a ((field_type (ReferenceKind Int257))))))
       (type_methods ())))) |}]

let%expect_test "function" =
  let source =
    {|
      fn test(a: Int257, b: Bool) -> Int257 {
       1;
       2;
      }
  |}
  in
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void) (println (Function (BuiltinFn <fun>)))
        (test
         (Function
          (Fn
           ((function_params ((a (BuiltinKind Int257)) (b (BuiltinKind Bool))))
            (function_returns (BuiltinKind Int257))
            (function_body ((Term (Integer 1)) (Term (Integer 2)))))))))))) |}]

let%expect_test "compile-time printing" =
  let source = {|
    let a = println(1);
   |} in
  pp_stripped source ;
  [%expect
    {|
    (Integer 1)
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void) (a Void) (println (Function (BuiltinFn <fun>))))))) |}]

let%expect_test "compile-time evaluation" =
  let source =
    {|
    fn a() -> Int257 {
       return 1;
    }
    let v = a();
   |}
  in
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void)
        (a
         (Function
          (Fn
           ((function_params ()) (function_returns (BuiltinKind Int257))
            (function_body ((Return (Integer 1))))))))
        (println (Function (BuiltinFn <fun>))) (v (Integer 1)))))) |}]

let%expect_test "parametric type instantiation" =
  let source = {|
      type T(A: Type) { a: A }
      let TA = T(Int257);
   |} in
  pp_stripped source ;
  [%expect
    {|
      (Ok
       ((scope
         ((Bool (Builtin Bool)) (Int257 (Builtin Int257))
          (T
           (Function
            (Fn
             ((function_params ((A (BuiltinKind Type))))
              (function_returns (BuiltinKind Type))
              (function_body
               ((Term
                 (Type
                  ((type_fields ((a ((field_type (ReferenceKind A))))))
                   (type_methods ()))))))))))
          (TA
           (Type
            ((type_fields
              ((a ((field_type (ResolvedReferenceKind A (BuiltinKind Int257)))))))
             (type_methods ()))))
          (Type (Builtin Type)) (Void Void) (println (Function (BuiltinFn <fun>)))))))
 |}]
