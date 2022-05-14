open Shared

let test_alias () =
  let source = {|
  struct T { val a: Int(257) }
  let T1 = T; 
  |} in
  Alcotest.(check bool)
    "aliased types are the same" true
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t1 =
       List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     in
     pp_sexp (Lang.sexp_of_expr t) ;
     pp_sexp (Lang.sexp_of_expr t1) ;
     Lang.equal_expr t t1 )

let test_carbon_copy () =
  let source =
    {|
  struct T { val a: Int(257) }
  struct T1 { val a: Int(257) }
  |}
  in
  Alcotest.(check bool)
    "carbon copy types are not the same" false
    (let scope = (compile source).bindings in
     let t = List.Assoc.find scope ~equal:String.equal "T" |> Option.value_exn
     and t1 =
       List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     in
     pp_sexp (Lang.sexp_of_expr t) ;
     pp_sexp (Lang.sexp_of_expr t1) ;
     Lang.equal_expr t t1 )

let test_parameterized () =
  let source =
    {|
  struct T(X: Type) { val a: X }
  let T1 = T(Int(257));
  let T2 = T(Bool);
  let T3 = T(Int(257));
  |}
  in
  Alcotest.(check bool)
    "differently parameterized types are not the same" false
    (let scope = (compile source).bindings in
     let t1 = List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     and t2 =
       List.Assoc.find scope ~equal:String.equal "T2" |> Option.value_exn
     in
     pp_sexp (Lang.sexp_of_expr t1) ;
     pp_sexp (Lang.sexp_of_expr t2) ;
     Lang.equal_expr t1 t2 ) ;
  Alcotest.(check bool)
    "equally parameterized types are the same" true
    (let scope = (compile source).bindings in
     let t1 = List.Assoc.find scope ~equal:String.equal "T1" |> Option.value_exn
     and t3 =
       List.Assoc.find scope ~equal:String.equal "T3" |> Option.value_exn
     in
     pp_sexp (Lang.sexp_of_expr t1) ;
     pp_sexp (Lang.sexp_of_expr t3) ;
     Lang.equal_expr t1 t3 )

let () =
  let open Alcotest in
  run "Lang Types"
    [ ( "equality",
        [ test_case "aliased type" `Quick test_alias;
          test_case "carbon copy (same definition)" `Quick test_carbon_copy;
          test_case "parameterized types" `Quick test_parameterized ] ) ]
