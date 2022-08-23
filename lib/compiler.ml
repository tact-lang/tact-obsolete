(* Compiler frontend *)

module Config = Located.Enabled
module Lang = Lang.Make (Config)
module Show = Show.Make (Config)
module Syntax = Syntax.Make (Config)
module Parser = Parser.Make (Config)
module Codegen_func = Codegen_func.Make (Config)
module Builtin = Builtin.Make (Config)

let rec compile ?(codegen_impl = Codegen_func.codegen) ?(filename = "<unnamed>")
    ch =
  let text = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  compile_from_string ~codegen_impl ~filename text

and compile_from_string ?(codegen_impl = Codegen_func.codegen)
    ?(filename = "<unnamed>") src =
  codegen ~codegen_impl (compile_to_ir ~filename src)

and codegen ?(codegen_impl = Codegen_func.codegen) program =
  match program with
  | Ok program' ->
      let generated_code = codegen_impl program' and buffer = Buffer.create 0 in
      let formatter = Caml.Format.formatter_of_buffer buffer in
      Func.pp_program formatter generated_code ;
      Ok (Buffer.contents buffer)
  | Error e ->
      Error e

and eval_stmt ~(constructor : _ Lang.constructor) ~filename text =
  ignore filename ;
  match
    MParser.(
      parse_string
        Parser.(
          handle_errors
            ( attempt (locate stmt)
            <|> ( locate expr
                |>> fun s -> Syntax.map_located s ~f:(fun _ -> Syntax.Expr s) )
          <|> locate (return (Syntax.CodeBlock [])) )
            ) )
        text () 
  with
  | Success stx -> (
      let errors = constructor#get_errors in
      let stmt =
        constructor#visit_located constructor#visit_stmt Lang.default_ctx stx
      in
      let result =
        (constructor#make_interpreter stmt.span)#interpret_stmt
          (Syntax.map_located
             ~f:(function Lang.Expr e -> Lang.Return e | stmt -> stmt)
             stmt )
          []
      in
      match errors#to_result () with
      | Error _ ->
          Error (errors#show_errors text)
      | Ok _ ->
          Ok result )
  | MParser.Failed (msg, _) ->
      Error msg

and construct ?(prev_program = Lang.default_program ()) ~filename text =
  ignore filename ;
  let stx = Parser.parse text in
  let errors = new Errors.errors Show.show_error in
  (stx, new Lang.constructor ~program:prev_program errors)

and compile_to_ir' ?(prev_program = Lang.default_program ()) ~filename text =
  match construct ~prev_program ~filename text with
  | stx, constructor -> (
      let program = constructor#visit_program Lang.default_ctx stx in
      let errors = constructor#get_errors in
      match errors#to_result () with
      | Error _ ->
          Error (errors#show_errors text)
      | Ok _ ->
          Ok (constructor, program) )
  | exception Parser.Error (msg, _) ->
      Error msg

and compile_to_ir ?(prev_program = Lang.default_program ()) ~filename text =
  Result.map snd @@ compile_to_ir' ~prev_program ~filename text

let compile_with_std ?(codegen_impl = Codegen_func.codegen)
    ?(filename = "<unnamed>") ch =
  let prev_program =
    compile_to_ir ~filename:"std.tact" Builtin.std |> Result.get_ok
  in
  let text = really_input_string ch (in_channel_length ch) in
  close_in ch ;
  codegen ~codegen_impl (compile_to_ir ~prev_program ~filename text)
