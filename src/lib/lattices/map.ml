open Base
open Utils

module Make (D : sig
  include Comparable.S

  val to_string : t -> string
end) (B : sig
  val bottom_elems : Set.M(D).t
end)
(L : Sig.S) =
struct
  exception Incompatible_arguments of string

  type property = L.property Map.M(D).t

  let bottom =
    Set.fold
      ~f:(fun map x -> Map.set map ~key:x ~data:L.bottom)
      B.bottom_elems
      ~init:(Map.empty (module D))

  let lub x y =
    Map.mapi x ~f:(fun ~key ~data ->
        match Map.find y key with
        | Some data_y -> L.lub data data_y
        | None -> raise (Incompatible_arguments "different key set"))

  let equal x y =
    Map.for_alli x ~f:(fun ~key ~data ->
        match Map.find y key with
        | Some data_y -> L.equal data data_y
        | None -> raise (Incompatible_arguments "different key set"))

  let to_string x =
    let f (key, data) = [%string "%{key#D}: %{data#L}"] in
    Map.to_string ~fst:"[" ~lst:"]" ~sep:";" ~f x

  let set x key data = Map.set x ~key ~data

  let get x key = Map.find x key
end
