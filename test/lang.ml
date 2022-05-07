module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
open Lang
open Core

let make_errors () = make_error_list ~warnings:(ref []) ~errors:(ref []) ()

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let default_env = {scope = Lang.empty_scope}

let result_of_errors elist t =
  match (!(elist.errors), !(elist.warnings)) with
  | [], _ ->
      Ok t
  | errors, _ ->
      Error errors

let build_program ?(env = default_env) stx =
  let elist = make_errors () in
  let env = env_from_program ~env stx elist in
  result_of_errors elist env

let print_sexp e =
  Sexplib.Sexp.pp_hum Format.std_formatter
    (Result.sexp_of_t Lang.sexp_of_env (List.sexp_of_t Lang.sexp_of_error) e)

let pp ?(env = default_env) s =
  let errors = make_errors () in
  Result.bind
    (parse_program s |> build_program ~env)
    ~f:(fun env ->
      let env = Lang.eval_env env errors in
      result_of_errors errors env )
  |> print_sexp

let pp_stripped ?(env = default_env) s =
  let errors = make_errors () in
  Result.bind
    (parse_program s |> build_program ~env)
    ~f:(fun env ->
      let env = (new resolved_references_stripper env)#visit_env () env in
      let env = Lang.eval_env env errors in
      result_of_errors errors env )
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
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

let%expect_test "deep resolution" =
  let source = {|
      let T = struct {a: Int257};
      let T1 = T;
    |} in
  pp source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257))
        (T
         (Struct
          ((struct_fields
            ((a
              ((field_type (ResolvedReferenceType Int257 (BuiltinType Int257)))))))
           (struct_methods ()) (id <opaque>))))
        (T1
         (ResolvedReference T
          (Struct
           ((struct_fields
             ((a
               ((field_type (ResolvedReferenceType Int257 (BuiltinType Int257)))))))
            (struct_methods ()) (id <opaque>)))))
        (Type (Builtin Type)) (Void Void)
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

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
        (n (Integer 1)) (n_ (Integer 1))
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

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
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>)))))
        (test
         (Function
          (Fn
           ((function_params
             ((i (ResolvedReferenceType Int257 (BuiltinType Int257)))))
            (function_returns (ResolvedReferenceType Void VoidType))
            (function_impl (((Term (Reference i))))))))))))) |}]

let%expect_test "recursive scope resolution" =
  let source = {|
  let A = B;
  let B = C;
  let C = A;
  |} in
  pp source ;
  [%expect
    {|
    (Error
     ((Recursive_Reference A) (Unresolved B) (Recursive_Reference B)
      (Unresolved C) (Recursive_Reference C) (Unresolved A))) |}]

let%expect_test "struct definition" =
  let source =
    {|
  let MyType = struct {
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
         (Struct
          ((struct_fields
            ((a ((field_type (BuiltinType Int257))))
             (b ((field_type (BuiltinType Bool))))))
           (struct_methods ()) (id <opaque>))))
        (Type (Builtin Type)) (Void Void)
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

let%expect_test "duplicate type" =
  let source = {|
  let MyType = struct {};
  let MyType = struct {};
  |} in
  pp source ;
  [%expect
    {|
    (Error
     ((Duplicate_Identifier MyType
       (Struct ((struct_fields ()) (struct_methods ()) (id <opaque>)))))) |}]

let%expect_test "duplicate but of a different kind" =
  let source = {|
  let MyType = 1;
  let MyType = struct {};
  |} in
  pp source ;
  [%expect {|
    (Error ((Duplicate_Identifier MyType (Integer 1)))) |}]

let%expect_test "duplicate type field" =
  let source =
    {|
  let MyType = struct {
      a: Int257,
      a: Bool
  };
  |}
  in
  pp source ;
  [%expect
    {|
    (Error
     ((Duplicate_Field a
       ((struct_fields ((a ((field_type (ReferenceType Int257))))))
        (struct_methods ()) (id <opaque>))))) |}]

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
        (Void Void)
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>)))))
        (test
         (Function
          (Fn
           ((function_params ((a (BuiltinType Int257)) (b (BuiltinType Bool))))
            (function_returns (BuiltinType Int257))
            (function_impl (((Term (Integer 1)) (Term (Integer 2))))))))))))) |}]

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
        (Void Void) (a Void)
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

let%expect_test "compile-time evaluation" =
  let source =
    {|
    fn a(i: Int257) -> Int257 {
       f(i);
       i
    }
    let v = a(1);
    let v1 = a(2);
   |}
  in
  let received = ref [] in
  let f _env args =
    received := args @ !received ;
    Void
  in
  let env =
    { scope =
        ( "f",
          Function
            (BuiltinFn
               { function_params = [("i", BuiltinType "Int257")];
                 function_returns = VoidType;
                 function_impl = f } ) )
        :: default_env.scope }
  in
  pp_stripped ~env source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void)
        (a
         (Function
          (Fn
           ((function_params ((i (BuiltinType Int257))))
            (function_returns (BuiltinType Int257))
            (function_impl
             (((Term
                (FunctionCall
                 (Function
                  (BuiltinFn
                   ((function_params ((i (BuiltinType Int257))))
                    (function_returns VoidType) (function_impl <fun>))))
                 ((Reference i))))
               (Return (Reference i)))))))))
        (f
         (Function
          (BuiltinFn
           ((function_params ((i (BuiltinType Int257))))
            (function_returns VoidType) (function_impl <fun>)))))
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>)))))
        (v (Integer 1)) (v1 (Integer 2)))))) |}] ;
  let received =
    List.sort !received ~compare:(fun a b ->
        match (a, b) with Integer a, Integer b -> Z.compare a b | _ -> 0 )
  in
  Sexplib.Sexp.pp_hum Format.std_formatter
    (List.sexp_of_t Lang.sexp_of_term received) ;
  [%expect {| ((Integer 1) (Integer 2)) |}]

let%expect_test "parametric struct instantiation" =
  let source =
    {|
      struct T(A: Type) { a: A }
      let TA = T(Int257);
   |}
  in
  pp_stripped source ;
  [%expect
    {|
      (Ok
       ((scope
         ((Bool (Builtin Bool)) (Int257 (Builtin Int257))
          (T
           (Function
            (Fn
             ((function_params ((A (BuiltinType Type))))
              (function_returns (BuiltinType Type))
              (function_impl
               (((Term
                  (Struct
                   ((struct_fields ((a ((field_type (ReferenceType A))))))
                    (struct_methods ()) (id <opaque>)))))))))))
          (TA
           (Struct
            ((struct_fields ((a ((field_type (BuiltinType Int257))))))
             (struct_methods ()) (id <opaque>))))
          (Type (Builtin Type)) (Void Void)
          (println
           (Function
            (BuiltinFn
             ((function_params ((value HoleType))) (function_returns VoidType)
              (function_impl <fun>)))))))))
 |}]

let%expect_test "function without a return type" =
  let source = {|
    fn f() { 1 }
    let a = f();
    |} in
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void) (a (Integer 1))
        (f
         (Function
          (Fn
           ((function_params ()) (function_returns HoleType)
            (function_impl (((Return (Integer 1)))))))))
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]

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
  pp_stripped source ;
  [%expect
    {|
    (Ok
     ((scope
       ((Bool (Builtin Bool)) (Int257 (Builtin Int257)) (Type (Builtin Type))
        (Void Void) (b (Integer 1))
        (f
         (Function
          (Fn
           ((function_params ((i (BuiltinType Int257))))
            (function_returns HoleType)
            (function_impl (((Let a (Reference i) ((Return (Reference a)))))))))))
        (println
         (Function
          (BuiltinFn
           ((function_params ((value HoleType))) (function_returns VoidType)
            (function_impl <fun>))))))))) |}]
