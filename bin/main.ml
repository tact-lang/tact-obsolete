open Base
module Show = Tact.Compiler.Show
module Lang = Tact.Compiler.Lang
module Syntax = Tact.Compiler.Syntax
module Builtin = Tact.Compiler.Builtin

let interpret_file argv =
  let filename = Array.get argv 1 in
  match Tact.Compiler.compile ~filename (Caml.open_in filename) with
  | Ok program ->
      Caml.Format.print_string program
  | Error errors ->
      Caml.Format.print_string errors

let prompt = "# "

let default_program () =
  let program = Lang.default_program () in
  Result.ok_or_failwith
  @@ Tact.Compiler.compile_to_ir ~filename:"std.tact" ~prev_program:program
       Builtin.std

let rec repl ?(program = default_program ()) ?(prompt = prompt) () =
  match LNoise.linenoise prompt with
  | None ->
      ()
  | Some input -> (
      ignore @@ LNoise.history_add input ;
      match
        Tact.Compiler.compile_to_ir ~filename:"<stdin>" ~prev_program:program
          input
      with
      | Ok ({result = Some result; _} as program) ->
          Show.pp_value Caml.Format.std_formatter result ;
          Caml.Format.print_newline () ;
          Caml.Format.print_flush () ;
          repl ~program ~prompt ()
      | Ok ({bindings; _} as program') ->
          List.iter bindings ~f:(fun (name, value) ->
              if
                Option.is_none
                @@ List.find program.bindings ~f:(fun (name', value') ->
                       Syntax.equal_located String.equal name name'
                       && Syntax.equal_located Lang.equal_expr_kind value value' )
              then (
                Caml.Format.print_string @@ Syntax.value name ;
                Caml.Format.print_string " = " ;
                Show.pp_expr Caml.Format.std_formatter value ;
                Caml.Format.print_newline () ) ) ;
          Caml.Format.print_flush () ;
          repl ~program:program' ~prompt ()
      | Error errors ->
          Caml.Format.print_string errors ;
          Caml.Format.print_flush () ;
          repl ~prompt () )

let () =
  let argv = Sys.get_argv () in
  if Array.length argv > 2 then interpret_file argv
  else LNoise.set_multiline true ;
  ignore @@ LNoise.history_set ~max_length:1000 ;
  repl ()
