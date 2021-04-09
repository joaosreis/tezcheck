type lower_bound = LInf | LInt of int

type upper_bound = HInf | HInt of int

type property = lower_bound * upper_bound

let bottom = (LInt 0, HInt 0)

let is_bottom = ( = ) bottom

let equal = ( = )

let is_maximal = ( = ) (LInf, HInf)

let lub (l1, h1) (l2, h2) = (min l1 l2, max h1 h2)

let to_string (l, h) =
  let l_string = match l with LInf -> "-inf" | LInt x -> string_of_int x in
  let h_string = match h with HInf -> "+inf" | HInt x -> string_of_int x in
  Printf.sprintf "(%s,%s)" l_string h_string

(* TODO implement operations *)
