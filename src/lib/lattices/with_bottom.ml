type 'a t = Bottom | Some of 'a

module Make (L : Sig.S) = struct
  type element = L.property

  type property = element t

  let bottom = Bottom

  let equal x y =
    match (x, y) with
    | Bottom, Bottom -> true
    | Some _, Bottom | Bottom, Some _ -> false
    | Some a, Some b -> L.equal a b

  let lub x y =
    match (x, y) with
    | Bottom, a | a, Bottom -> a
    | Some a, Some b -> Some (L.lub a b)

  let to_string = function Bottom -> "_" | Some x -> L.to_string x
end
