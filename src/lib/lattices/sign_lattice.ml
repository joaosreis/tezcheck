type sign = Zero | Zero_pos | Zero_neg | One | Pos | Neg

type property = Bottom | Top | Sign of sign

let is_maximal = function Top -> true | _ -> false

let equal x y =
  match (x, y) with
  | Bottom, Bottom | Top, Top -> true
  | Sign x, Sign y -> x = y
  | _ -> false

let bottom = Bottom

let lub x y =
  match (x, y) with
  | Bottom, a | a, Bottom -> a
  | Top, _ | _, Top -> Top
  | Sign x, Sign y -> (
      match (x, y) with
      | One, One -> Sign One
      | One, Pos | Pos, One -> Sign Pos
      | One, Zero | Zero, One -> Sign Zero_pos
      | One, Neg | Neg, One -> Top
      | One, Zero_pos | Zero_pos, One -> Sign Zero_pos
      | One, Zero_neg | Zero_neg, One -> Top
      | Pos, Pos -> Sign Pos
      | Pos, Zero | Zero, Pos -> Sign Zero_pos
      | Pos, Neg | Neg, Pos -> Top
      | Pos, Zero_pos | Zero_pos, Pos -> Sign Zero_pos
      | Pos, Zero_neg | Zero_neg, Pos -> Top
      | Zero, Zero -> Sign Zero
      | Zero, Zero_pos | Zero_pos, Zero -> Sign Zero_pos
      | Zero, Zero_neg | Zero_neg, Zero -> Sign Zero_neg
      | Zero, Neg | Neg, Zero -> Sign Zero_neg
      | Neg, Zero_pos | Zero_pos, Neg -> Top
      | Neg, Zero_neg | Zero_neg, Neg -> Sign Zero_neg
      | Neg, Neg -> Sign Neg
      | Zero_pos, Zero_pos -> Sign Zero_pos
      | Zero_pos, Zero_neg | Zero_neg, Zero_pos -> Top
      | Zero_neg, Zero_neg -> Sign Zero_neg )

let to_string = function
  | Bottom -> "bottom"
  | Top -> "top"
  | Sign Zero -> "0"
  | Sign Zero_pos -> "+0"
  | Sign Zero_neg -> "-0"
  | Sign One -> "1"
  | Sign Pos -> "+"
  | Sign Neg -> "-"

let neg = function
  | Sign Pos | Sign One -> Sign Neg
  | Sign Neg -> Sign Pos
  | Sign Zero_pos -> Sign Zero_neg
  | Sign Zero_neg -> Sign Zero_pos
  | (Sign Zero | Top | Bottom) as x -> x

let plus x y =
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top -> Top
  | Sign x, Sign y -> (
      match (x, y) with
      | Zero, z | z, Zero -> Sign z
      | Pos, Neg
      | Neg, Pos
      | Zero_pos, Zero_neg
      | Zero_neg, Zero_pos
      | Pos, Zero_neg
      | Zero_neg, Pos
      | Zero_pos, Neg
      | Neg, Zero_pos ->
          Top
      | Neg, Neg | Zero_neg, Neg | Neg, Zero_neg -> Sign Neg
      | Pos, Pos | Zero_pos, Pos | Pos, Zero_pos -> Sign Pos
      | Zero_pos, Zero_pos -> Sign Zero_pos
      | Zero_neg, Zero_neg -> Sign Zero_neg
      | Zero_neg, One -> Top
      | Zero_pos, One -> Sign Pos
      | Pos, One -> Sign Pos
      | Neg, One -> Sign Zero_neg
      | One, Zero_pos -> Sign Pos
      | One, Zero_neg -> Top
      | One, One -> Sign Pos
      | One, Pos -> Sign Pos
      | One, Neg -> Sign Zero_pos )

let minus x y =
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top -> Top
  | Sign x, Sign y -> (
      match (x, y) with
      | z, Zero -> Sign z
      | Zero, z -> neg (Sign z)
      | Pos, Pos -> Top
      | Pos, Zero_pos -> Top
      | Pos, Zero_neg -> Sign Pos
      | Pos, One -> Sign Zero_pos
      | Pos, Neg -> Sign Pos
      | Zero_pos, Pos -> Top
      | Zero_pos, Zero_pos -> Top
      | Zero_pos, Zero_neg -> Sign Zero_pos
      | Zero_pos, One -> Top
      | Zero_pos, Neg -> Sign Pos
      | Zero_neg, Pos -> Sign Neg
      | Zero_neg, Zero_pos -> Sign Zero_neg
      | Zero_neg, Zero_neg -> Top
      | Zero_neg, One -> Sign Neg
      | Zero_neg, Neg -> Top
      | One, Pos -> Sign Zero_neg
      | One, Zero_pos -> Top
      | One, Zero_neg -> Sign Zero_pos
      | One, One -> Sign Zero
      | One, Neg -> Sign Pos
      | Neg, Pos -> Sign Neg
      | Neg, Zero_pos -> Sign Neg
      | Neg, Zero_neg -> Top
      | Neg, One -> Sign Neg
      | Neg, Neg -> Top )

let times x y =
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | Sign Zero, _ | _, Sign Zero -> Sign Zero
  | Top, _ | _, Top -> Top
  | Sign x, Sign y -> (
      match (x, y) with
      | Zero, _ | _, Zero -> Sign Zero
      | z, One | One, z -> Sign z
      | Pos, Pos | Neg, Neg -> Sign Pos
      | Pos, Neg | Neg, Pos -> Sign Neg
      | Pos, Zero_pos -> Sign Zero_pos
      | Pos, Zero_neg -> Sign Zero_neg
      | Neg, Zero_pos -> Sign Zero_neg
      | Neg, Zero_neg -> Sign Zero_pos
      | Zero_pos, Zero_pos -> Sign Zero_pos
      | Zero_pos, Zero_neg -> Sign Zero_neg
      | Zero_pos, Pos -> Sign Zero_pos
      | Zero_pos, Neg -> Sign Zero_neg
      | Zero_neg, Zero_pos -> Sign Zero_neg
      | Zero_neg, Zero_neg -> Sign Zero_pos
      | Zero_neg, Pos -> Sign Zero_neg
      | Zero_neg, Neg -> Sign Zero_pos )

let divide x y =
  match (x, y) with
  | Bottom, _ | _, Bottom -> Bottom
  | _, Sign Zero -> Bottom
  | Sign Zero, _ -> Sign Zero
  | Top, _ | _, Top -> Top
  | Sign x, Sign y -> (
      match (x, y) with
      | _, Zero -> Bottom
      | z, One -> Sign z
      | _, Zero_pos -> Top
      | _, Zero_neg -> Top
      | Zero, _ -> Sign Zero
      | Pos, Pos -> Sign Pos
      | Pos, Neg -> Sign Neg
      | Zero_pos, Pos -> Sign Zero_pos
      | Zero_pos, Neg -> Sign Zero_neg
      | Zero_neg, Pos -> Sign Zero_neg
      | Zero_neg, Neg -> Sign Zero_pos
      | One, Pos -> Sign Zero
      | One, Neg -> Sign Zero_neg
      | Neg, Pos -> Sign Zero_pos
      | Neg, Neg -> Sign Zero_pos )

let sign i =
  if Z.(i = zero) then Sign Zero
  else if Z.(i = one) then Sign One
  else if Z.(gt i zero) then Sign Pos
  else Sign Neg
