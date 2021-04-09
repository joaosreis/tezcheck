type 'a t = Top | Some of 'a

module Make (L : Sig.S) = struct
  type property = L.property t

  let bottom = Some L.bottom

  let equal x y =
    match (x, y) with
    | Top, Top -> true
    | Some _, Top | Top, Some _ -> false
    | Some a, Some b -> L.equal a b

  let lub x y =
    match (x, y) with
    | Some a, Some b -> Some (L.lub a b)
    | Top, _ | _, Top -> Top

  let to_string = function Top -> "¯" | Some x -> L.to_string x
end
