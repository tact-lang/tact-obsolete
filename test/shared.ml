module Make =
functor
  (Config : Tact.Config.T)
  ->
  struct
    module Syntax = Tact.Syntax.Make (Config)
    module Parser = Tact.Parser.Make (Config)
    module Lang = Tact.Lang.Make (Config)
    module Show = Tact.Show.Make (Config)
    module Interpreter = Tact.Interpreter.Make (Config)
    module Builtin = Tact.Builtin.Make (Config)
    module Codegen = Tact.Codegen_func.Make (Config)
    module Errors = Tact.Errors
    module Zint = Tact.Zint
    module Func = Tact.Func
    include Core

    type error = [Lang.error | Interpreter.error] [@@deriving sexp_of]

    let make_errors e = new Errors.errors e

    let parse_program s = Parser.parse s

    let strip_if_exists_in_other o1 o2 ~equal =
      List.filter o1 ~f:(fun o1_item ->
          not @@ List.exists o2 ~f:(equal o1_item) )

    let strip : program:Lang.program -> previous:Lang.program -> Lang.program =
     fun ~program ~previous ->
      { program with
        bindings =
          strip_if_exists_in_other program.bindings previous.bindings
            ~equal:(fun (x1, _) (y1, _) ->
              Config.equal_located equal_string x1 y1 );
        structs =
          strip_if_exists_in_other program.structs previous.structs
            ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
        unions =
          strip_if_exists_in_other program.unions previous.unions
            ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
        interfaces =
          strip_if_exists_in_other program.interfaces previous.interfaces
            ~equal:(fun (id1, _) (id2, _) -> equal_int id1 id2);
        struct_signs =
          Lang.Arena.strip_if_exists program.struct_signs previous.struct_signs;
        union_signs =
          Lang.Arena.strip_if_exists program.union_signs previous.union_signs }

    let compile_pass p prev_program errors =
      let c = new Lang.constructor ~program:prev_program errors in
      let p' = c#visit_program Lang.default_ctx p in
      p'

    let build_program ?(errors = make_errors Show.show_error)
        ?(prev_program = Lang.default_program ()) ?(strip_defaults = true)
        ~include_std ~codegen p =
      let prev_prog =
        match include_std with
        | true ->
            let c = new Lang.constructor ~program:prev_program errors in
            let p' =
              c#visit_program Lang.default_ctx (parse_program Builtin.std)
            in
            p'
        | false ->
            prev_program
      in
      (* This will make a deep copy. Lang.constructor mutates input program,
         so we need deep copy if we want to strip bindings later. *)
      let prev_prog_copy =
        { prev_prog with
          bindings = prev_prog.bindings;
          struct_signs = Lang.Arena.deep_copy prev_prog.struct_signs;
          union_signs = Lang.Arena.deep_copy prev_prog.union_signs }
      in
      let p' = compile_pass p prev_prog_copy errors in
      let p'' =
        if strip_defaults then strip ~program:p' ~previous:prev_prog else p'
      in
      errors#to_result p''
      |> Result.map_error ~f:(fun errors ->
             let errs = List.map errors ~f:(fun (_, err, _) -> err) in
             (errs, p'') )
      |> Result.map ~f:codegen

    let rec pp_sexp = Sexplib.Sexp.pp_hum Caml.Format.std_formatter

    and sexp_of_errors =
      sexp_of_pair (List.sexp_of_t sexp_of_error) Lang.sexp_of_program

    and print_sexp e =
      pp_sexp (Result.sexp_of_t Lang.sexp_of_program sexp_of_errors e)

    let pp_compile ?(prev_program = Lang.default_program ())
        ?(strip_defaults = true) ?(include_std = true)
        ?(show_errors = fun x _ -> pp_sexp (sexp_of_errors x)) s =
      parse_program s
      |> build_program ~prev_program ~strip_defaults ~include_std
           ~codegen:(fun x -> x)
      |> fun res ->
      ( match res with
      | Ok t ->
          pp_sexp @@ Result.sexp_of_t Lang.sexp_of_program sexp_of_errors (Ok t)
      | Error e ->
          show_errors e s ) ;
      Caml.print_newline ()

    let pp_codegen ?(prev_program = Lang.default_program ())
        ?(strip_defaults = false) ?(include_std = true) s =
      let _ =
        parse_program s
        |> build_program ~prev_program ~strip_defaults ~include_std
             ~codegen:Codegen.codegen
        |> Result.map ~f:(Func.pp_program Caml.Format.std_formatter)
        |> Result.map_error ~f:(fun e -> pp_sexp (sexp_of_errors e))
      in
      ()

    exception Exn of error list * Lang.program

    let compile s =
      parse_program s
      |> build_program ~codegen:(fun x -> x) ~include_std:true
      |> Result.map_error ~f:(fun (errs, p) -> Exn (errs, p))
      |> Result.ok_exn
  end

module EnabledConfig = struct
  include Tact.Located.Enabled
end

module DisabledConfig = struct
  include Tact.Located.Disabled
end

module Enabled = Make (EnabledConfig)
module Disabled = Make (DisabledConfig)
