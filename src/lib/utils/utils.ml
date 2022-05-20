open! Core
module StringMap = Map.Make (String)

let cartesian_product xs ys =
  List.concat (List.map ~f:(fun x -> List.map ~f:(fun y -> (x, y)) ys) xs)

let fst3 (x, _, _) = x

let trd (_, _, x) = x

let ( --- ) lower upper =
  let rec helper u i = if i > u then [] else i :: helper u (i + 1) in
  helper upper lower

let ( -- ) lower upper = lower --- (upper - 1)

let dualize f (xs, ys) = (f xs, f ys)

let dual_map f (xs, ys) = dualize (List.map ~f) (xs, ys)

let dual_fold_left f acc (xs, ys) =
  dualize (List.fold_left ~f ~init:acc) (xs, ys)

let pair x y = (x, y)

let rev_pair x y = (y, x)

module List = struct
  include List

  let to_string ~f ?(fst = "") ?sep ?(lst = "") x =
    let l = List.map ~f x in
    let l = fst :: l @ [ lst ] in
    match sep with None -> String.concat l | Some sep -> String.concat ~sep l
end

module Set = struct
  include Set

  let to_string ~f ?(fst = "") ?sep ?(lst = "") x =
    let l = List.map ~f (Set.to_list x) in
    let l = fst :: l @ [ lst ] in
    match sep with None -> String.concat l | Some sep -> String.concat ~sep l
end

module Map = struct
  include Map

  let to_string ~f ?(fst = "") ?sep ?(lst = "") x =
    let l = List.map ~f (Map.to_alist x) in
    let l = fst :: l @ [ lst ] in
    match sep with None -> String.concat l | Some sep -> String.concat ~sep l
end
