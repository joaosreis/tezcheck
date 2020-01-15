open Batteries
open Utils

module Uniform_product_lattice(L : Sig.Lattice)(N : sig val n : int end) : Sig.Lattice = struct
  type property     = L.property list

  let bottom        = List.make N.n L.bottom
  let lub x y       = List.map2 (L.lub) x y
  let equal         = (=)
  let to_string     = sprint_list L.to_string
  let is_maximal _  = false
end

module Pair_lattice(L1 : Sig.Lattice)(L2 : Sig.Lattice) = struct
  type property     = L1.property * L2.property

  let bottom        = L1.bottom, L2.bottom
  let lub x y       = L1.lub (fst x) (fst y), L2.lub (snd x) (snd y)
  let equal         = (=)
  let to_string x   = Printf.sprintf "%s, %s" (fst x |> L1.to_string) (snd x |> L2.to_string)
  let is_maximal _  = false
end

module Powerset_lattice(D : Sig.Element) = struct
  type property = D.t Set.t

  let bottom        = Set.empty
  let lub           = Set.union
  let equal         = Set.equal
  let to_string     = sprint_set D.to_string
  let is_maximal _  = false
end

module Reverse_powerset_lattice
    (D : sig include Sig.Element val bottom : t Set.t end) = struct
  type property = D.t Set.t

  let bottom        = D.bottom
  let lub           = Set.intersect
  let equal         = Set.equal
  let to_string     = sprint_set D.to_string
  let is_maximal _  = false
end

module Map_lattice(D : sig include Sig.Element val bottom_elems : t Set.t end)
    (L : Sig.Lattice) = struct
  type property = (D.t, L.property) Map.t

  let bottom    =
    Set.fold (fun x map -> Map.add x L.bottom map) D.bottom_elems Map.empty
  let lub x y   = let open Map.Infix in
    Map.keys x |> Enum.fold (fun acc k -> acc <-- (k,L.lub (x --> k) (y --> k))) y
  let equal     = (=)
  let to_string = sprint_map D.to_string L.to_string
  let is_maximal _  = false

  let set x k v = Map.add k v x
  let get x k = Map.find k x
end

module Flat_lattice(X : Sig.Element) = struct
  type property = Bottom | Top | Element of X.t

  let lift x = Element x
  let unlift = function
      Element x     -> x
    | Bottom | Top  -> raise (Invalid_argument "cannot unlift")
  let bottom = Bottom
  let top = Top
  let is_maximal = (=) Top
  let lub x y = match x,y with
      Bottom,_ | _,Top    -> y
    | _ when x = y        -> y
    | _,Bottom | Top,_    -> x
    | Element _,Element _ -> Top
  let equal     = (=)
  let to_string = function
      Bottom    -> "bot"
    | Top       -> "top"
    | Element x -> X.to_string x
end

exception Bottom_exception

module Lattice_with_bottom(L : Sig.Lattice) = struct
  type element = L.property
  type property = Bottom | Some of element

  let bottom = Bottom

  let is_maximal = function
    | Bottom -> false
    | Some a -> L.is_maximal a

  let equal x y = match x, y with
      Bottom, Bottom -> true
    | Some _, Bottom
    | Bottom, Some _ -> false
    | Some a, Some b -> L.equal a b

  let lub x y = match x, y with
      Bottom, a
    | a, Bottom       -> a
    | Some a, Some b  -> Some (L.lub a b)

  let extract = function
      Bottom -> raise Bottom_exception
    | Some x -> x

  let to_string = function
      Bottom -> "_"
    | Some x -> L.to_string x
end

module Lattice_with_top(L : Sig.Lattice) =
struct
  type property = Top | Some of L.property

  let bottom = L.bottom

  let is_maximal = function
    | Top 	 -> true
    | Some _ -> false

  let equal x y = match (x, y) with
    | (Top, Top) 			 -> true
    | (Some _, Top)
    | (Top, Some _) 	 -> false
    | (Some a, Some b) -> L.equal a b

  let lub x y = match (x, y) with
    | (Some a, Some b) -> Some (L.lub a b)
    | (Top, _)
    | (_, Top) -> Top

  let to_string = function
    | Top ->"¯"
    | Some x -> L.to_string x
end

module Sequence_lattice(L : Sig.Lattice) =
struct
  type property = L.property list

  let bottom : property = []

  let is_maximal x = List.for_all L.is_maximal x

  let equal x y = List.length x = List.length y && List.for_all2 L.equal x y

  let rec lub a b = match (a, b) with
    | (x::xs, y::ys) -> (L.lub x y)::(lub xs ys)
    | (xs, [])
    | ([], xs)       -> xs

  let to_string x = List.map L.to_string x |> String.concat ", "
end

module Interval_lattice = struct
  type lower_bound = LInf | LInt of int
  type upper_bound = HInf | HInt of int
  type property = lower_bound * upper_bound

  let bottom = LInt 0, HInt 0
  let equal = (=)
  let is_maximal = (=) (LInf,HInf)

  let lub (l1, h1) (l2, h2) = (min l1 l2, max h1 h2)
  let to_string (l, h) =
    let l_string = match l with
        LInf    -> "-inf"
      | LInt x  -> string_of_int x
    in let h_string = match h with
        HInf    -> "+inf"
      | HInt x  -> string_of_int x in
    Printf.sprintf "(%s,%s)" l_string h_string

  (* TODO implement operations *)
end
