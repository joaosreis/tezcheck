module Make (L1 : Sig.S) (L2 : Sig.S) = struct
  type property = L1.property * L2.property

  let bottom = (L1.bottom, L2.bottom)

  let lub x y = (L1.lub (fst x) (fst y), L2.lub (snd x) (snd y))

  let equal = ( = )

  let to_string x =
    Printf.sprintf "%s, %s" (fst x |> L1.to_string) (snd x |> L2.to_string)

  let fst = fst

  let snd = snd

  let of_pair x = x
end
