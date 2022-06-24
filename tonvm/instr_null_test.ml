let%expect_test "null" =
  Tests.(execute "(NULL)" |> print_stack) ;
  [%expect {| (Null) |}]

let%expect_test "null check" =
  Tests.(execute "(NULL ISNULL)" |> print_stack) ;
  [%expect {| ((Integer -1)) |}]

let%expect_test "negative null check" =
  Tests.(execute "(NULL ISNULL ISNULL)" |> print_stack) ;
  [%expect {| ((Integer 0)) |}]
