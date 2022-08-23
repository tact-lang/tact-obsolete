open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    module LocalDiscriminators = struct
      type t = unit

      open struct
        let get_discr_from_attrs attrs =
          List.find_map attrs ~f:(fun {attribute_ident; attribute_exprs} ->
              match (attribute_ident.value, attribute_exprs) with
              | "discriminator", [{value = Value (Integer x); _}] ->
                  Some (Discriminator {discr = Z.to_int x; bits = None})
              | ( "discriminator",
                  [ {value = Value (Integer x); _};
                    {value = Value (Integer bits); _} ] ) ->
                  Some
                    (Discriminator
                       {discr = Z.to_int x; bits = Some (Z.to_int bits)} )
              | _ ->
                  None )
      end

      let choose_discriminators :
          t ->
          int ->
          (type_ * attribute list) list ->
          (type_ * discriminator) list =
       fun _ _ cases ->
        List.mapi (List.rev cases) ~f:(fun id (case, attrs) ->
            ( case,
              Option.value_or_thunk (get_discr_from_attrs attrs)
                ~default:(fun _ -> Discriminator {discr = id; bits = None}) ) )
        |> List.rev
    end
  end
