open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    let bl = Config.builtin_located

    let derive_executor p _exprs = function
      | ImplInput {impl; self_ty} -> (
        match impl.mk_impl_interface with
        (* Serialize intf *)
        | {value = Value (Type (InterfaceType -1)); _} ->
            let builder_ty =
              List.find_map_exn p.bindings ~f:(fun (n, e) ->
                  if String.equal n.value "Builder" then Some e else None )
              |> expr_to_type p
            in
            let self_serializer =
              FunctionCall
                ( bl @@ Reference (bl "serializer", HoleType),
                  [bl @@ Value (Type self_ty)] )
            in
            let fun_body =
              bl
              @@ Return
                   ( bl
                   @@ FunctionCall
                        ( bl @@ self_serializer,
                          [ bl @@ Reference (bl "self", self_ty);
                            bl @@ Reference (bl "b", builder_ty) ] ) )
            in
            let function_signature =
              bl
              @@ { function_params = [(bl "self", self_ty); (bl "b", builder_ty)];
                   function_returns = builder_ty;
                   function_attributes = [] }
            in
            let method_ =
              bl
              @@ MkFunction
                   (bl {function_signature; function_impl = Fn fun_body})
            in
            let impl =
              { mk_impl_interface = impl.mk_impl_interface;
                mk_impl_attributes = [];
                mk_impl_methods = [(bl "serialize", method_)] }
            in
            ImplInput {impl; self_ty}
        (* Deserialize intf *)
        | {value = Value (Type (InterfaceType -2)); _} ->
            ImplInput {impl; self_ty}
        | _ ->
            raise Errors.InternalCompilerError )

    let attr_executors = [("derive", derive_executor)]
  end
