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
module C = Tact.Compiler
include Core

type error = [Lang.error | Interpreter.error] * Lang.program
[@@deriving sexp_of]

let make_errors e = new Errors.errors e

let parse_program s = Parser.program Tact.Lexer.token (Lexing.from_string s)

let build_program ?(errors = make_errors Show.show_error)
    ?(prev_program = Lang.default_program ()) ?(strip_defaults = true) p =
  let std =
    let c = new Lang.constructor ~program:prev_program errors in
    let p' = c#visit_program () (parse_program Tact.Builtin.std) in
    p'
  in
  let c = new Lang.constructor ~program:std errors in
  let p' = c#visit_program () p in
  errors#to_result ()
  |> Result.map ~f:(fun _ -> p')
  (* remove default bindings and methods *)
  |> Result.map ~f:(fun (program : Lang.program) ->
         if strip_defaults then
           { program with
             bindings =
               List.filter program.bindings ~f:(fun binding ->
                   not
                   @@ List.exists std.bindings ~f:(Lang.equal_binding binding) );
             structs =
               List.filter program.structs ~f:(fun (id1, _) ->
                   not
                   @@ List.exists prev_program.structs ~f:(fun (id2, _) ->
                          equal_int id1 id2 ) );
             unions =
               List.filter program.unions ~f:(fun (id1, _) ->
                   not
                   @@ List.exists prev_program.unions ~f:(fun (id2, _) ->
                          equal_int id1 id2 ) );
             interfaces =
               List.filter program.interfaces ~f:(fun (id1, _) ->
                   not
                   @@ List.exists prev_program.interfaces ~f:(fun (id2, _) ->
                          equal_int id1 id2 ) ) }
         else program )
  |> Result.map_error ~f:(fun errors ->
         List.map errors ~f:(fun (_, err, _) ->
             ( err,
               if strip_defaults then
                 { p' with
                   bindings =
                     List.filter p'.bindings ~f:(fun binding ->
                         not
                         @@ List.exists std.bindings
                              ~f:(Lang.equal_binding binding) );
                   structs =
                     List.filter p'.structs ~f:(fun (id1, _) ->
                         not
                         @@ List.exists prev_program.structs ~f:(fun (id2, _) ->
                                equal_int id1 id2 ) );
                   unions =
                     List.filter p'.unions ~f:(fun (id1, _) ->
                         not
                         @@ List.exists prev_program.unions ~f:(fun (id2, _) ->
                                equal_int id1 id2 ) );
                   interfaces =
                     List.filter p'.interfaces ~f:(fun (id1, _) ->
                         not
                         @@ List.exists prev_program.interfaces
                              ~f:(fun (id2, _) -> equal_int id1 id2) ) }
               else p' ) ) )

let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

and print_sexp e =
  pp_sexp
    (Result.sexp_of_t Lang.sexp_of_program (List.sexp_of_t sexp_of_error) e)

let pp ?(prev_program = Lang.default_program ()) s =
  parse_program s |> build_program ~prev_program |> print_sexp

exception Exn of error list

let compile s =
  parse_program s |> build_program
  |> Result.map_error ~f:(fun err -> Exn err)
  |> Result.ok_exn
