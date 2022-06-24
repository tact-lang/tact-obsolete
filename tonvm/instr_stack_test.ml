let%expect_test "NOP" =
  Tests.(execute "(NOP)" |> print_stack) ;
  [%expect {| () |}]

let%expect_test "SWAP" =
  Tests.(execute "(NULL DUP ISNULL SWAP)" |> print_stack) ;
  [%expect {| (Null (Integer -1)) |}]

let%expect_test "XCHG0" =
  Tests.(execute "(NULL DUP ISNULL (XCHG0 1))" |> print_stack) ;
  [%expect {| (Null (Integer -1)) |}]

let%expect_test "XCHG" =
  Tests.(execute "(NULL DUP DUP ISNULL (XCHG (0 2)))" |> print_stack) ;
  [%expect {| (Null Null (Integer -1)) |}]

let%expect_test "DUP" =
  Tests.(execute "(NULL DUP)" |> print_stack) ;
  [%expect {| (Null Null) |}]

let%expect_test "OVER" =
  Tests.(execute "(NULL DUP ISNULL OVER)" |> print_stack) ;
  [%expect {| (Null (Integer -1) Null) |}]

let%expect_test "POP" =
  Tests.(execute "(NULL DUP ISNULL (POP 1))" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]

let%expect_test "DROP" =
  Tests.(execute "(NULL DUP ISNULL DROP)" |> print_stack) ;
  [%expect {| (Null) |}]

let%expect_test "NIP" =
  Tests.(execute "(NULL DUP ISNULL NIP)" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]
