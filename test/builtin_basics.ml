open Shared.Disabled
module Config = Shared.DisabledConfig

let find scope name =
  let open Config in
  List.find_map scope ~f:(fun (n, ex) ->
      if equal_string n.value name then Some ex else None )

let%test "int type equality" =
  let source =
    {|
  let T = Int(257);
  let T1 = Int(257);
  let T2 = Int(256);
  |}
  in
  Alcotest.(check bool)
    "types with same bits are equal" true
    (let scope = (compile source).bindings in
     let t = find scope "T" |> Option.value_exn
     and t1 = find scope "T1" |> Option.value_exn in
     pp_sexp (Lang.sexp_of_expr t) ;
     pp_sexp (Lang.sexp_of_expr t1) ;
     Lang.equal_expr t t1 ) ;
  Alcotest.(check bool)
    "types with different bits are not equal" false
    (let scope = (compile source).bindings in
     let t = find scope "T" |> Option.value_exn
     and t2 = find scope "T2" |> Option.value_exn in
     pp_sexp (Lang.sexp_of_expr t) ;
     pp_sexp (Lang.sexp_of_expr t2) ;
     Lang.equal_expr t t2 )
