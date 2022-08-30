module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    open Config

    open struct
      let make_state_deserialization_stmt span =
        let sp x = {value = x; span} in
        DestructuringLet
          { destructuring_let =
              [(sp "state", sp "state"); (sp "slice", sp "slice")];
            destructuring_let_expr = sp @@ Value Void;
            destructuring_let_rest = false }
    end

    let make_internal_messages_handler func =
      match func.function_signature.value.function_params with
      | [(_, state_ty); (_, msg_body_type)] ->
          Ok ()
      | [(_, state_ty); (_, msg_body_type); (_, rest_slice_ty)] ->
          Ok ()
      | _ ->
          Error (func.function_signature.span, "Invalid number of arguments")
  end