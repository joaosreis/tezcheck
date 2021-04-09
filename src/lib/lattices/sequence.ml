module Make (L : Sig.S) = struct
  type property = L.property list

  let bottom : property = []

  let equal x y = List.length x = List.length y && List.for_all2 L.equal x y

  let rec lub a b =
    match (a, b) with
    | x :: xs, y :: ys -> L.lub x y :: lub xs ys
    | xs, [] | [], xs -> xs

  let to_string x = List.map L.to_string x |> String.concat ", "
end
