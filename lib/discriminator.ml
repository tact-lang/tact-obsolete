open Base

module Make =
functor
  (Config : Config.T)
  ->
  struct
    open Lang_types.Make (Config)

    module LocalDiscriminators = struct
      type t = unit

      let choose_discriminators :
          t -> int -> type_ list -> (type_ * discriminator) list =
       fun _ _ cases ->
        List.mapi (List.rev cases) ~f:(fun id case -> (case, Discriminator id))
        |> List.rev
    end
  end
