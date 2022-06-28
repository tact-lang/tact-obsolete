let%expect_test "NOP" =
  Tests.(execute "(NOP)" |> print_stack) ;
  [%expect {| () |}]

let%expect_test "NOP encoding" =
  Tests.(encode_decode `NOP |> print) ;
  [%expect {| (Ok NOP) |}]

let%expect_test "SWAP" =
  Tests.(execute "(NULL DUP ISNULL SWAP)" |> print_stack) ;
  [%expect {| (Null (Integer -1)) |}]

let%expect_test "XCHG0" =
  Tests.(execute "(NULL DUP ISNULL (XCHG0 1))" |> print_stack) ;
  [%expect {| (Null (Integer -1)) |}]

let%expect_test "XCHG" =
  Tests.(execute "(NULL DUP DUP ISNULL (XCHG (0 2)))" |> print_stack) ;
  [%expect {| (Null Null (Integer -1)) |}]

let%expect_test "XCHG encoding" =
  Tests.(encode_decode (`XCHG (0, 1)) |> print) ;
  Tests.(encode_decode (`XCHG (1, 2)) |> print) ;
  Tests.(encode_decode (`XCHG (0, 100)) |> print) ;
  [%expect
    {|
    (Ok (XCHG (0 1)))
    (Ok (XCHG (1 2)))
    (Ok (XCHG (0 100))) |}]

let%expect_test "DUP" =
  Tests.(execute "(NULL DUP)" |> print_stack) ;
  [%expect {| (Null Null) |}]

let%expect_test "OVER" =
  Tests.(execute "(NULL DUP ISNULL OVER)" |> print_stack) ;
  [%expect {| (Null (Integer -1) Null) |}]

let%expect_test "POP" =
  Tests.(execute "(NULL DUP ISNULL (POP 1))" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]

let%expect_test "POP encoding" =
  Tests.(encode_decode (`POP 1) |> print) ;
  [%expect {|
    (Ok (POP 1)) |}]

let%expect_test "DROP" =
  Tests.(execute "(NULL DUP ISNULL DROP)" |> print_stack) ;
  [%expect {| (Null) |}]

let%expect_test "NIP" =
  Tests.(execute "(NULL DUP ISNULL NIP)" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]

let%expect_test "PUSH" =
  Tests.(execute "(NULL (PUSH 0))" |> print_stack) ;
  [%expect {| (Null Null) |}]

let%expect_test "PUSH encoding" =
  Tests.(encode_decode (`PUSH 1) |> print) ;
  [%expect {|
    (Ok (PUSH 1)) |}]
