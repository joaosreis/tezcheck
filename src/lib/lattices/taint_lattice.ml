include Lattices.Flat_lattice (struct
  type t = bool

  let to_string = string_of_bool
end)

let join_taint x y =
  match (x, y) with
  | Top, _ | _, Top -> Top
  | Element true, _ | _, Element true -> Element true
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> Element false
