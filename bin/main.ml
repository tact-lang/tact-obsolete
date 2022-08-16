open Core
open Bos
module Show = Tact.Compiler.Show
module Lang = Tact.Compiler.Lang
module Syntax = Tact.Compiler.Syntax
module Builtin = Tact.Compiler.Builtin

let toolchain_available () =
  let exists f =
    match OS.Cmd.exists @@ Cmd.v f with Ok true -> true | _ -> false
  in
  match (exists "func", exists "fift") with
  | _, _ ->
      Option.is_some @@ Sys.getenv "FIFTPATH"
  | exception _ ->
      false

let stdlib_fc = [%blob "stdlib.fc"]

let compile filename program =
  let cell_file =
    (Filename.chop_extension filename |> Filename.basename) ^ ".cell"
  in
  ignore
  @@ OS.File.with_tmp_oc
       (format_of_string "tact_%s")
       (fun path out () ->
         Stdio.Out_channel.(
           output_string out stdlib_fc ;
           output_string out program ;
           flush out ) ;
         let output =
           OS.Cmd.(
             run_out Cmd.(v "func" % "-APS" % Fpath.to_string path) |> to_string )
         in
         match output with
         | Ok fift ->
             let fift = fift ^ "\nboc>B \"" ^ cell_file ^ "\" B>file\n" in
             ( ignore
             @@ OS.Cmd.(in_string fift |> run_io Cmd.(v "fift") |> to_null) ) ;
             Caml.Format.print_string ("Compiled to " ^ cell_file) ;
             Caml.Format.print_newline ()
         | _ ->
             Caml.Format.print_string "Failed to use the toolchain" ;
             Caml.Format.print_newline () )
       () ;
  ()

let interpret_file argv =
  let filename = Array.get argv 1 in
  match Tact.Compiler.compile_with_std ~filename (Caml.open_in filename) with
  | Ok program ->
      if toolchain_available () then compile filename program
      else Caml.Format.print_string program
  | Error errors ->
      Caml.Format.print_string errors

let prompt = "# "

let default_program () =
  let program = Lang.default_program () in
  Result.ok_or_failwith @@ Result.map ~f:fst
  @@ Tact.Compiler.compile_to_ir' ~filename:"std.tact" ~prev_program:program
       Builtin.std

let rec repl ?(program = default_program ()) ?(prompt = prompt) () =
  match LNoise.linenoise prompt with
  | None ->
      ()
  | Some input -> (
      let bindings = program#bindings in
      ignore @@ LNoise.history_add input ;
      match
        Tact.Compiler.eval_stmt ~constructor:program ~filename:"<stdin>" input
      with
      | Ok result when not @@ Lang.equal_value result Void ->
          Show.pp_value Caml.Format.std_formatter result ;
          Caml.Format.print_newline () ;
          Caml.Format.print_flush () ;
          repl ~program ~prompt ()
      | Ok _ ->
          List.iter program#bindings ~f:(fun (name, value) ->
              if
                Option.is_none
                @@ List.find bindings ~f:(fun (name', value') ->
                       Syntax.equal_located String.equal name name'
                       && Syntax.equal_located Lang.equal_expr_kind value value' )
              then (
                Caml.Format.print_string @@ Syntax.value name ;
                Caml.Format.print_string " = " ;
                Show.pp_expr Caml.Format.std_formatter value ;
                Caml.Format.print_newline () ) ) ;
          Caml.Format.print_flush () ;
          repl ~program ~prompt ()
      | Error errors ->
          Caml.Format.print_string errors ;
          Caml.Format.print_flush () ;
          repl ~prompt () )

let () =
  let argv = Sys.get_argv () in
  if Array.length argv > 1 then interpret_file argv
  else (
    LNoise.set_multiline true ;
    ignore @@ LNoise.history_set ~max_length:1000 ;
    repl () )
