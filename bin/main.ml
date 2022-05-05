open Printf
module E = MenhirLib.ErrorReports
module L = MenhirLib.LexerUtil
module Syntax = Tact.Syntax.Make (Tact.Located.Disabled)
module Parser = Tact.Parser.Make (Syntax)

let fastpath filename =
  let text, lexbuf = L.read filename in
  match Parser.program Tact.Lexer.token lexbuf with
  | result ->
      Sexplib.Sexp.pp_hum Format.std_formatter (Syntax.sexp_of_program result) ;
      exit 0
  | exception Tact.Lexer.Error msg ->
      eprintf "lexing error: %s" msg ;
      exit 1
  | exception Parser.Error ->
      text

module I = Tact.UnitActionsParser.MenhirInterpreter

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      eprintf "State: %d\n" (I.current_state_number env) ;
      env
  | _ ->
      assert false

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)
let show text positions =
  E.extract text positions |> E.sanitize |> E.compress
  |> E.shorten 20 (* max width 43 *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
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

let state checkpoint : int =
  match I.top (env checkpoint) with
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

let fail text buffer (checkpoint : _ I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = Tact.ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message ;
  exit 1

let slowpath filename text =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = L.init filename (Lexing.from_string text) in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Tact.Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint =
    Tact.UnitActionsParser.Incremental.program lexbuf.lex_curr_p
  in
  (* Run the parser. *)
  (* We do not handle [Lexer.Error] because we know that we will not
     encounter a lexical error during this second parsing run. *)
  I.loop_handle succeed (fail text buffer) supplier checkpoint

let () =
  let filename = Sys.argv.(1) in
  let text = fastpath filename in
  slowpath filename text
