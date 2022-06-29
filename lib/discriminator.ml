open Lang_types
open Base

module LocalDiscriminators = struct
  type t = unit

  let choose_discriminators :
      t -> int -> type_ list -> (type_ * discriminator) list =
   fun _ _ cases ->
    List.mapi (List.rev cases) ~f:(fun id case ->
        (case, Lang_types.Discriminator id) )
    |> List.rev
end
