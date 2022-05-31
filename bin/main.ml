open Base

let () =
  let filename = Array.get (Sys.get_argv ()) 1 in
  match Tact.Compiler.compile ~filename (Caml.open_in filename) with
  | Ok program ->
      Caml.Format.print_string program
  | Error errors ->
      Caml.Format.print_string errors
