let%expect_test "NULL" =
  Tests.(execute "(NULL)" |> print_stack) ;
  [%expect {| (Null) |}]

let%expect_test "NULL encoding" =
  Tests.(encode_decode `NULL |> print) ;
  [%expect {| (Ok NULL) |}]

let%expect_test "ISNULL" =
  Tests.(execute "(NULL ISNULL)" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]

let%expect_test "negative ISNULL" =
  Tests.(execute "(NULL ISNULL ISNULL)" |> print_stack) ;
  [%expect {| ((Integer 0)) |}]

let%expect_test "ISNULL encoding" =
  Tests.(encode_decode `ISNULL |> print) ;
  [%expect {| (Ok ISNULL) |}]
