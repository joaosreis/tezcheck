include Flat.Make (struct
  type t = bool

  let to_string = string_of_bool

  let compare = Bool.compare
end)

let join_taint x y =
  let open Flat in
  match (x, y) with
  | Top, _ | _, Top -> Top
  | Element true, _ | _, Element true -> Element true
  | Bottom, _ | _, Bottom -> Bottom
  | _ -> Element false
