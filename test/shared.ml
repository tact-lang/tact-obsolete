module Config = struct
  include Tact.Located.Disabled
end

module Syntax = Tact.Syntax.Make (Config)
module Parser = Tact.Parser.Make (Config)
module Lang = Tact.Lang.Make (Config)
module Show = Tact.Show.Make (Config)
module Interpreter = Tact.Interpreter
module Errors = Tact.Errors
module Zint = Tact.Zint
include Core

type error = [Lang.error | Interpreter.error] * Lang.program
[@@deriving sexp_of]

let make_errors e = new Errors.errors e

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors Show.show_error)
    ?(bindings = Lang.default_bindings) ?(methods = Lang.default_methods)
    ?(strip_defaults = true) p =
  let c = new Lang.constructor bindings methods errors in
  let p' = c#visit_program () p in
  errors#to_result p'
  (* remove default bindings and methods *)
  |> Result.map ~f:(fun (program : Lang.program) : Lang.program ->
         if strip_defaults then
           { bindings =
               List.filter program.bindings ~f:(fun binding ->
                   not @@ List.exists bindings ~f:(Lang.equal_binding binding) )
           }
         else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) -> (err, p')) )

let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

and print_sexp e =
  pp_sexp
    (Result.sexp_of_t Lang.sexp_of_program (List.sexp_of_t sexp_of_error) e)

let pp ?(bindings = Lang.default_bindings) s =
  parse_program s |> build_program ~bindings |> print_sexp

exception Exn of error list

let compile s =
  parse_program s |> build_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn
