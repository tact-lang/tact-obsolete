(* Compiler frontend *)

open struct
  open Printf
  module I = UnitActionsParser.MenhirInterpreter
  module E = MenhirLib.ErrorReports
  module L = MenhirLib.LexerUtil
  module Config = Located.Enabled
  module Syntax = Syntax.Make (Config)
  module Parser = Parser.Make (Config)
  module Lang = Lang.Make (Config)
  module Show = Show.Make (Config)
  module Codegen_func = Codegen_func.Make (Config)

  (* [env checkpoint] extracts a parser environment out of a checkpoint,
     which must be of the form [HandlingError env]. *)

  let env ebuffer checkpoint =
    match checkpoint with
    | I.HandlingError env ->
        bprintf ebuffer "State: %d\n" (I.current_state_number env) ;
        env
    | _ ->
        assert false

  (* [show text (pos1, pos2)] displays a range of the input text [text]
     delimited by the positions [pos1] and [pos2]. *)
  let show text positions =
    E.extract text positions |> E.sanitize |> E.compress
    |> E.shorten 20 (* max width 43 *)

  let get ebuffer text checkpoint i =
    match I.get i (env ebuffer checkpoint) with
    | Some (I.Element (_, _, pos1, pos2)) ->
        show text (pos1, pos2)
    | None ->
        (* The index is out of range. This should not happen if [$i]
             keywords are correctly inside the syntax error message
             database. The integer [i] should always be a valid offset
             into the known suffix of the stack. *)
        "???"

  (* [state checkpoint] extracts the number of the current state out of a
     checkpoint. *)

  let state ebuffer checkpoint : int =
    match I.top (env ebuffer checkpoint) with
    | Some (I.Element (s, _, _, _)) ->
        I.number s
    | None ->
        (* Hmm... The parser is in its initial state. The incremental API
             currently lacks a way of finding out the number of the initial
             state. It is usually 0, so we return 0. This is unsatisfactory
             and should be fixed in the future. *)
        0

  (* [succeed v] is invoked when the parser has succeeded and produced a
     semantic value [v]. In our setting, this cannot happen, since the
     table-based parser is invoked only when we know that there is a
     syntax error in the input file. *)

  let succeed _v = assert false

  let fail ebuffer text buffer (checkpoint : _ I.checkpoint) =
    (* Indicate where in the input file the error occurred. *)
    let location = L.range (E.last buffer) in
    (* Show the tokens just before and just after the error. *)
    let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
    (* Fetch an error message from the database. *)
    let message = ParserMessages.message (state ebuffer checkpoint) in
    (* Expand away the $i keywords that might appear in the message. *)
    let message = E.expand (get ebuffer text checkpoint) message in
    (* Show these three components. *)
    bprintf ebuffer "%s%s%s%!" location indication message

  let slowpath ebuffer filename text =
    (* Allocate and initialize a lexing buffer. *)
    let lexbuf = L.init filename (Lexing.from_string text) in
    (* Wrap the lexer and lexbuf together into a supplier, that is, a
       function of type [unit -> token * position * position]. *)
    let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
    (* Equip the supplier with a two-place buffer that records the positions
       of the last two tokens. This is useful when a syntax error occurs, as
       these are the token just before and just after the error. *)
    let buffer, supplier = E.wrap_supplier supplier in
    (* Fetch the parser's initial checkpoint. *)
    let checkpoint = UnitActionsParser.Incremental.program lexbuf.lex_curr_p in
    (* Run the parser. *)
    (* We do not handle [Lexer.Error] because we know that we will not
       encounter a lexical error during this second parsing run. *)
    I.loop_handle succeed (fail ebuffer text buffer) supplier checkpoint
end

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

and compile_to_ir ?(prev_program = Lang.default_program ()) ~filename text =
  let lexbuf = L.init filename @@ Lexing.from_string text in
  match Parser.program Lexer.token lexbuf with
  | stx -> (
      let errors = new Errors.errors Show.show_error in
      let constructor = new Lang.constructor ~program:prev_program errors in
      let program = constructor#visit_program () stx in
      match errors#to_result () with
      | Error _ ->
          Error errors#show_errors
      | Ok _ ->
          Ok program )
  | exception Lexer.Error msg ->
      Error ("lexing error: " ^ msg)
  | exception Parser.Error ->
      let buffer = Buffer.create 0 in
      slowpath buffer filename text ;
      Error (Buffer.contents buffer)
