open Base

module Make
    (L : Sig.S) (N : sig
      val n : int
    end) =
struct
  exception Unequal_lengths

  type property = L.property list

  let bottom = List.init N.n ~f:(fun _ -> L.bottom)

  let lub x y =
    match List.map2 ~f:L.lub x y with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> raise Unequal_lengths

  let equal x y =
    match List.for_all2 ~f:L.equal x y with
    | List.Or_unequal_lengths.Ok x -> x
    | List.Or_unequal_lengths.Unequal_lengths -> raise Unequal_lengths

  let to_string x =
    let l = [ "]" ] in
    let l = "[" :: List.map ~f:L.to_string x @ l in
    String.concat ~sep:";" l
end
