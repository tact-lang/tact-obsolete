open Base

class ['a, 's, 'e, 'm] errors =
  object (s : _)
    val mutable errors = []

    method report : 's -> 'e -> 'm -> unit =
      fun severity error meta -> errors <- (severity, error, meta) :: errors

    method errors = List.rev errors

    method to_result : 'a -> ('a, _) Result.t =
      fun value -> if List.is_empty errors then Ok value else Error s#errors
  end
