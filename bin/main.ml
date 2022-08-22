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

let compile ?(target = `Cell) filename program =
  let ext =
    match target with `Cell -> ".cell" | `Func -> ".fc" | `Fift -> ".fif"
  and is_func = match target with `Func -> true | _ -> false
  and is_fift = match target with `Fift -> true | _ -> false in
  let out_file =
    (Filename.chop_extension filename |> Filename.basename) ^ ext
  in
  if is_func then (
    ignore
    @@ OS.File.with_oc
         (Option.value_exn @@ Result.ok @@ Fpath.of_string out_file)
         (fun out () ->
           Stdio.Out_channel.(output_string out program ; flush out) ;
           Ok () )
         () ;
    Caml.Format.print_string ("Compiled to " ^ out_file) ;
    Caml.Format.print_newline () )
  else
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
               run_out Cmd.(v "func" % "-APS" % Fpath.to_string path)
               |> to_string )
           in
           match output with
           | Ok fift ->
               let fift = fift ^ "\nboc>B \"" ^ out_file ^ "\" B>file\n" in
               ( if is_fift then
                 ignore
                 @@ OS.File.with_oc
                      (Option.value_exn @@ Result.ok @@ Fpath.of_string out_file)
                      (fun out () ->
                        Stdio.Out_channel.(output_string out fift ; flush out) ;
                        Ok () )
                      ()
               else
                 ignore
                 @@ OS.Cmd.(in_string fift |> run_io Cmd.(v "fift") |> to_null)
               ) ;
               Caml.Format.print_string ("Compiled to " ^ out_file) ;
               Caml.Format.print_newline ()
           | _ ->
               Caml.Format.print_string "Failed to use the toolchain" ;
               Caml.Format.print_newline () )
         () ;
  ()

let interpret_file ~target filename =
  match Tact.Compiler.compile_with_std ~filename (Caml.open_in filename) with
  | Ok program -> (
      let is_toolchain_available = toolchain_available () in
      match (is_toolchain_available, target) with
      | _, `Func ->
          compile ~target filename program
      | false, _ ->
          Caml.Format.print_string
            "Can't compile for this target without a toolchain"
      | true, target ->
          compile ~target filename program )
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

open Cmdliner

let tact target filename =
  match (target, filename) with
  | _, None ->
      LNoise.set_multiline true ;
      ignore @@ LNoise.history_set ~max_length:1000 ;
      repl ()
  | target, Some filename ->
      interpret_file ~target filename

let tact_t =
  let target =
    let doc = "Output target [cell, func, fift] (ignored for REPL)" in
    Arg.(
      value
      & opt (enum [("cell", `Cell); ("func", `Func); ("fift", `Fift)]) `Cell
      & info ["t"; "target"] ~doc )
  and filename =
    let doc = "Input filename, if none given, will start REPL" in
    Arg.(
      value
      & pos ~rev:true 0 (some non_dir_file) None
      & info [] ~doc ~docv:"FILE" )
  in
  Term.(const tact $ target $ filename)

let cmd =
  let doc = "compile Tact smart contract" in
  let info = Cmd.info "tact" ~doc in
  Cmd.v info tact_t

let main () = exit (Cmd.eval cmd)

let () = main ()
