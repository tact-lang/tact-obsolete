module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
module Codegen = Tact.Codegen.Demo_FunC
include Core

type error = [Lang.error | Interpreter.error] [@@deriving sexp_of]

let make_errors () = new Errors.errors

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors ()) ?(bindings = Lang.default_bindings)
    ?(strip_default_bindings = true) p =
  let c = new Lang.constructor (bindings, errors) in
  let p' = c#visit_program () p in
  errors#to_result p'
  (* remove default bindings *)
  |> Result.map ~f:(fun (program : Lang.program) ->
         if strip_default_bindings then
           (* FIXME: stuck with the type inference *)
           let p : Lang.program =
             { bindings =
                 List.filter program.bindings ~f:(fun binding ->
                     not @@ List.exists bindings ~f:(Lang.equal_binding binding) )
             }
           in
           p
         else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> err) )

let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

and print_sexp e =
  pp_sexp
    (Result.sexp_of_t Lang.sexp_of_program (List.sexp_of_t sexp_of_error) e)

exception Exn of error list

let pp ?(bindings = Lang.default_bindings) s =
  parse_program s |> build_program ~bindings
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn |> Codegen.codegen_prog
  |> Codegen.pp_program Format.std_formatter

let%expect_test "Simple struct" =
  let source =
    {|
    struct Foo { val field1: Int(257) val field2: Int(256) }
  |}
  in
  pp source ;
  [%expect
    {|
    cell serialize_int256(builder b, int value) {
    builder b =
    store_int(b, value, 256);
    return
    b;
    }
    cell serialize_int256_cell(int value) {
    builder b =
    new_builder();
    builder b =
    serialize_int256(b, value);
    return
    build(b);
    }
    cell serialize_int257(builder b, int value) {
    builder b =
    store_int(b, value, 257);
    return
    b;
    }
    cell serialize_int257_cell(int value) {
    builder b =
    new_builder();
    builder b =
    serialize_int257(b, value);
    return
    build(b);
    }
    builder serialize_struct0(builder b, int field1, int field2) {
    builder b =
    serialize_int257(b, field1);
    builder b =
    serialize_int256(b, field2);
    return
    b;
    }
    cell serialize_struct0_cell(int field1, int field2) {
    builder b =
    new_builder();
    builder b =
    serialize_struct0(b, field1, field2);
    return
    build(b);
    } |}]
