open Base

exception InternalCompilerError

class ['a, 's, 'e, 'm] errors (show_error : string -> 'e -> string) =
  object (s : _)
    val mutable errors = []

    val show_error = show_error

    method report : 's -> 'e -> 'm -> unit =
      fun severity error meta -> errors <- (severity, error, meta) :: errors

    method errors = List.rev errors

    method to_result : 'a -> ('a, _) Result.t =
      fun value -> if List.is_empty errors then Ok value else Error s#errors

    method show_errors code =
      List.fold_left (List.rev errors) ~init:"" ~f:(fun s (_, e, _) ->
          s ^ show_error code e ^ "\n" )
  end
