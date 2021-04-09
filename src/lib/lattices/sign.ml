type sign = Zero | Pos | Neg

include Flat.Make (struct
  type t = sign

  let compare x y =
    match (x, y) with
    | Zero, Zero | Pos, Pos | Neg, Neg -> 0
    | Neg, (Zero | Pos) -> -1
    | Pos, (Zero | Neg) -> 1
    | Zero, Pos -> -1
    | Zero, Neg -> 1

  let to_string = function Zero -> "0" | Pos -> "+" | Neg -> "-"
end)

let plus x y =
  let open Flat in
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top | Element Pos, Element Neg | Element Neg, Element Pos -> Top
  | Element Zero, _ -> y
  | _, Element Zero -> x
  | Element Neg, Element Neg -> Element Neg
  | Element Pos, Element Pos -> Element Pos

let minus x y =
  let open Flat in
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top | Element Pos, Element Pos | Element Neg, Element Neg -> Top
  | Element Zero, Element Neg | Element Pos, Element Neg -> Element Pos
  | Element Zero, Element Pos | Element Neg, Element Pos -> Element Neg
  | _, Element Zero -> x

let times x y =
  let open Flat in
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Element Zero, _ | _, Element Zero -> Element Zero
  | Top, _ | _, Top -> Top
  | Element Pos, Element Pos | Element Neg, Element Neg -> Element Pos
  | Element Pos, Element Neg | Element Neg, Element Pos -> Element Neg

let divide x y =
  let open Flat in
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top | _, Element Zero | Element Pos, _ | Element Neg, _ -> Top
  | Element Zero, _ -> Element Zero

let sign i =
  let open Flat in
  if i = 0 then Element Zero else if i > 0 then Element Pos else Element Neg

let sig_bignum i =
  let open Flat in
  Bigint.(
    if i = zero then Element Zero
    else if i > zero then Element Pos
    else Element Neg)
