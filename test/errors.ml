open Tact.Errors

type severity = [`Warning | `Error]

let test_report_poly () =
  let e = new errors in
  Alcotest.(check bool)
    "reports gets recorded" true
    ( e#report `Warning `Broken () ;
      e#report `Error (`VeryBroken 1) () ;
      [%matches? [(`Warning, `Broken, ()); (`Error, `VeryBroken 1, ())]]
        e#errors )

let () =
  let open Alcotest in
  run "Errors"
    [("reporting", [test_case "polymorphic types" `Quick test_report_poly])]
