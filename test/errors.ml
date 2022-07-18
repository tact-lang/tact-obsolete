module Config = Shared.EnabledConfig
module Show = Tact.Show.Make (Shared.EnabledConfig)
open Config

let fmt = Caml.Format.std_formatter

let%expect_test "error showing one line" =
  let source = {|
fn test() {

}
|} in
  let open Show.DiagnosticMsg in
  let pos1 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 3 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let pos2 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 7 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let msg =
    { severity = `Error;
      diagnostic_id = 0;
      diagnostic_msg = "msg";
      spans = [(span_of_concrete (pos1, pos2), "message")];
      additional_msg = [] }
  in
  Show.DiagnosticMsg.show fmt msg source ;
  [%expect
    {|
    Error[0]: msg
    File: "f":1:3
      |
    1 | fn test() {
      |    ^^^^ message |}]

let%expect_test "error showing two lines" =
  let source = {|
let a = test(
  arg1, arg2);
|} in
  let open Show.DiagnosticMsg in
  let pos1 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 8 + 1; pos_lnum = 1; pos_bol = 1}
  in
  let pos2 : Tact.Located.pos =
    {pos_fname = "f"; pos_cnum = 13 + 13 + 1; pos_lnum = 2; pos_bol = 13 + 1}
  in
  let msg =
    { severity = `Error;
      diagnostic_id = 0;
      diagnostic_msg = "msg";
      spans = [(span_of_concrete (pos1, pos2), "message")];
      additional_msg = [] }
  in
  Show.DiagnosticMsg.show fmt msg source ;
  [%expect
    {|
    Error[0]: msg
    File: "f":1:8
      |
    1 | let a = test(...
      |         ^^^^^^^^ message |}]

let pp =
  let open Base in
  Shared.Enabled.pp_compile ~show_errors:(fun (elist, _) source ->
      List.iter elist ~f:(fun x ->
          let s = Show.show_error source x in
          Caml.Format.print_string s ) )

let%expect_test "failed scope resolution" =
  let source = {|
    let T = Int256;
  |} in
  pp source
