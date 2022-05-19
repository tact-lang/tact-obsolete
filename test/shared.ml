module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)
module Lang = Tact.Lang.Make (Syntax)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
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

let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

and print_sexp e =
  pp_sexp
    (Result.sexp_of_t Lang.sexp_of_program (List.sexp_of_t sexp_of_error) e)

let print_pp e =
  match e with
  | Ok e ->
      Tact.Pp.pp_program Caml.Format.std_formatter e
  | Error e ->
      print_sexp (Error e)

let pp ?(bindings = Lang.default_bindings) s =
  parse_program s |> build_program ~bindings |> print_pp

exception Exn of error list

let compile s =
  parse_program s |> build_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn
