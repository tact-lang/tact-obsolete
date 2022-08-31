open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    open Interpreter.Make (Config)

    open Errors
    open Config
    (*
       Received:
         fn recv(self: Self, msg: Msg) { ... }

       Output:
         fn recv(self: Self, msg: Msg) { ...; return self; }
         fn recv_internal(body: Slice) {
           let {value, slice} = Msg.deserialize(body);
           let state = deserializer[Self](Slice.parse(Globals.load_state())).value;
           let new_state = recv(state, msg);
           Globals.save_state(serializer[Self](new_state, Builder.new()).build());
         }
    *)

    open struct
      let insert_return_self span stmts =
        let sp x = {value = x; span} in
        stmts @ [sp @@ Return (sp @@ Reference (sp "self", HoleType))]

      (* fn recv(self: Self, msg: Msg) { ...; return self; } *)
      let update_fn_insert_return_self span fn ~state_ty =
        let sp x = {value = x; span} in
        match fn.function_impl with
        | Fn {value = Block stmts; _} ->
            { function_signature =
                { fn.function_signature with
                  value =
                    { fn.function_signature.value with
                      function_returns = state_ty } };
              function_impl = Fn (sp @@ Block (insert_return_self span stmts))
            }
        | _ ->
            unreachable ()

      (* let {value, slice} = Msg.deserialize(body); *)
      let make_msg_deserialization_stmt p span msg_ty =
        let sp x = {value = x; span} in
        let msg_deserialize_fn =
          Program.find_method p msg_ty "deserialize"
          |> Option.value_exn
          |> fun func -> {value = Value (Function func); span}
        in
        DestructuringLet
          { destructuring_let =
              [(sp "value", sp "value"); (sp "slice", sp "slice")];
            destructuring_let_rest = false;
            destructuring_let_expr =
              sp
                (FunctionCall
                   ( msg_deserialize_fn,
                     [sp @@ Reference (sp "body", HoleType)],
                     false ) ) }

      (* let state = deserializer[Self](Slice.parse(Globals.load_state())).value; *)
      let make_state_deserialization_stmt p errors span state_ty =
        let sp x = {value = x; span} in
        let deserialize_state_fn =
          let deserializer = Program.find_binding_exn p "deserializer" in
          let func =
            (new interpreter (make_ctx p (ref []) 0) errors span (fun _ x -> x))
              #interpret_fc
              (deserializer, [sp @@ Value (Type state_ty)], true)
          in
          {value = Value func; span}
        in
        let slice_parse_fn =
          Program.find_binding_exn p "Slice"
          |> fun x ->
          match x.value with
          | Value (Type ty) ->
              Program.find_method p ty "parse"
              |> Option.value_exn
              |> fun func -> {value = Value (Function func); span}
          | _ ->
              unreachable ()
        in
        let load_state_fn =
          Program.find_binding_exn p "Globals"
          |> fun x ->
          match x.value with
          | Value (Type ty) ->
              Program.find_method p ty "load_state"
              |> Option.value_exn
              |> fun func -> {value = Value (Function func); span}
          | _ ->
              unreachable ()
        in
        let call =
          sp
            (FunctionCall
               ( deserialize_state_fn,
                 [ sp
                     (FunctionCall
                        ( slice_parse_fn,
                          [sp (FunctionCall (load_state_fn, [], false))],
                          false ) ) ],
                 false ) )
        in
        let get_value = sp (StructField (call, sp "value", state_ty)) in
        Let [(sp "state", get_value)]

      (* let new_state = recv(state, msg); *)
      let call_recv_function span fn ~state_ty =
        let sp x = {value = x; span} in
        Let
          [ ( sp "new_state",
              sp
                (FunctionCall
                   ( fn,
                     [ sp @@ Reference (sp "state", state_ty);
                       sp @@ Reference (sp "value", HoleType) ],
                     false ) ) ) ]

      (* Globals.save_state(serializer[Self](new_state, Builder.new()).build()); *)
      let save_state p errors span ~state_ty =
        let sp x = {value = x; span} in
        let builder_new_fn =
          Program.find_binding_exn p "Builder"
          |> fun x ->
          match x.value with
          | Value (Type ty) ->
              Program.find_method p ty "new"
              |> Option.value_exn
              |> fun func -> {value = Value (Function func); span}
          | _ ->
              unreachable ()
        in
        let serialize_state_fn =
          let deserializer = Program.find_binding_exn p "serializer" in
          let func =
            (new interpreter (make_ctx p (ref []) 0) errors span (fun _ x -> x))
              #interpret_fc
              (deserializer, [sp @@ Value (Type state_ty)], true)
          in
          {value = Value func; span}
        in
        let builder_build_fn =
          Program.find_binding_exn p "Builder"
          |> fun x ->
          match x.value with
          | Value (Type ty) ->
              Program.find_method p ty "build"
              |> Option.value_exn
              |> fun func -> {value = Value (Function func); span}
          | _ ->
              unreachable ()
        in
        let save_state_fn =
          Program.find_binding_exn p "Globals"
          |> fun x ->
          match x.value with
          | Value (Type ty) ->
              Program.find_method p ty "save_state"
              |> Option.value_exn
              |> fun func -> {value = Value (Function func); span}
          | _ ->
              unreachable ()
        in
        Expr
          ( sp
          @@ FunctionCall
               ( save_state_fn,
                 [ sp
                   @@ FunctionCall
                        ( builder_build_fn,
                          [ sp
                            @@ FunctionCall
                                 ( serialize_state_fn,
                                   [ sp @@ Reference (sp "new_state", state_ty);
                                     sp
                                     @@ FunctionCall (builder_new_fn, [], false)
                                   ],
                                   false ) ],
                          false ) ],
                 false ) )

      let generate_recv_internal_fn p errors span ~msg_ty ~state_ty ~user_fn =
        let sp x = {value = x; span} in
        let deserialize_msg =
          sp @@ make_msg_deserialization_stmt p span msg_ty
        in
        let deserialize_state =
          sp @@ make_state_deserialization_stmt p errors span state_ty
        in
        let call_recv = sp @@ call_recv_function span user_fn ~state_ty in
        let save_state = sp @@ save_state p errors span ~state_ty in
        let slice_type = Program.find_binding_exn p "Slice" |> expr_to_type p in
        sp
        @@ { function_signature =
               sp
               @@ { function_params = [(sp "body", slice_type)];
                    function_returns = VoidType;
                    function_attributes = [];
                    function_is_type = false };
             function_impl =
               Fn
                 ( sp
                 @@ Block
                      [deserialize_msg; deserialize_state; call_recv; save_state]
                 ) }
    end

    let make_internal_messages_handler p errors func =
      let span = func.span in
      let sp x = {value = x; span} in
      match func.value.function_signature.value.function_params with
      | [(_, state_ty); (_, msg_body_type)] ->
          let new_fn =
            { func with
              value = update_fn_insert_return_self span func.value ~state_ty }
          in
          Ok
            (generate_recv_internal_fn p errors span ~msg_ty:msg_body_type
               ~state_ty
               ~user_fn:(sp @@ Value (Function new_fn)) )
      | [(_, _state_ty); (_, _msg_body_type); (_, _rest_slice_ty)] ->
          ice "Rest slice not supported yet"
      | _ ->
          Error
            (func.value.function_signature.span, "Invalid number of arguments")
  end
