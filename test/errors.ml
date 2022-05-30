open Tact.Errors

type severity = [`Warning | `Error]

let show_e _ = ""

let%test "error reporting" =
  let e = new errors show_e in
  Alcotest.(check bool)
    "reports gets recorded" true
    ( e#report `Warning `Broken () ;
      e#report `Error (`VeryBroken 1) () ;
      [%matches? [(`Warning, `Broken, ()); (`Error, `VeryBroken 1, ())]]
        e#errors )
