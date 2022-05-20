module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
module Codegen = Tact.Codegen_func
module Func = Tact.Func
include Core

type error = [Lang.error | Interpreter.error] * Lang.program
[@@deriving sexp_of]

let make_errors () = new Errors.errors

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors ()) ?(bindings = Lang.default_bindings)
    ?(methods = Lang.default_methods) ?(strip_defaults = true) p =
  let c = new Lang.constructor bindings methods errors in
  let p' = c#visit_program () p in
  errors#to_result {p' with stmts = []}
  (* remove default bindings and methods *)
  |> Result.map ~f:(fun (program : Lang.program) ->
         if strip_defaults then
           { program with
             bindings =
               List.filter program.bindings ~f:(fun binding ->
                   not @@ List.exists bindings ~f:(Lang.equal_binding binding) );
             methods =
               List.filter program.methods ~f:(fun (rcvr, rmethods) ->
                   not
                   @@ List.exists methods ~f:(fun (rcvr', rmethods') ->
                          Lang.equal_value rcvr' rcvr
                          && List.equal
                               (fun (name, value) (name', value') ->
                                 String.equal name' name
                                 && Lang.equal_function_ value' value )
                               rmethods' rmethods ) ) }
         else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> (err, p')) )

exception Exn of error list

let pp ?(bindings = Lang.default_bindings) s =
  parse_program s |> build_program ~bindings
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn |> Codegen.codegen
  |> Func.pp_program Caml.Format.std_formatter

let%expect_test "simple function generation" =
  let source = {|
      fn test() -> Integer { return 0; }
    |} in
  pp source ; [%expect {|
    int test() {
      return 0;
    } |}]

let%expect_test "passing struct to function" =
  let source = {|
      struct T { 
       val a: Int(32)
       val b: Integer
       val c: struct { val d : Integer }
      }
      fn test(t: T) -> Integer { return 1; }
    |} in
  pp source ; [%expect {|
    int test([[int], int, [int]] t) {
      return 1;
    } |}]
