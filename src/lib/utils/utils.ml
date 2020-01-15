open Batteries

module Hashtbl = MyHashtbl

module Set = MySet

module StringMap = Map.Make(String)

let cartesian_product xs ys =
  List.concat (List.map (fun x -> List.map (fun y -> (x, y)) ys) xs)

let fst3 (x, _, _) = x
let trd (_, _, x) = x

let (---) lower upper =
  let rec helper u i =
    if i > u then [] else i :: (helper u (i + 1))
  in helper upper lower

let (--) lower upper = lower --- (upper - 1)

let dualize f (xs, ys) =
  f xs, f ys

let dual_map f (xs, ys) = dualize (List.map f) (xs, ys)

let dual_fold_left f acc (xs, ys) = dualize (List.fold_left f acc) (xs, ys)

let pair x y = (x,y)

let rev_pair x y = (y,x)

let sprint_list ?(first = "[") ?(last = "]") ?(sep = "; ") to_string l =
  let strout = BatIO.output_string () in
  List.print ~first:first ~last:last ~sep:sep
    (fun outch x -> to_string x |> String.print outch) strout l;
  BatIO.close_out strout

let sprint_set to_string s =
  let strout = BatIO.output_string () in
  Set.print (fun outch x -> to_string x |> String.print outch) strout s;
  BatIO.close_out strout

let sprint_map k_to_string v_to_string m =
  let strout = BatIO.output_string () in
  Map.print (fun outch k -> k_to_string k |> String.print outch)
    (fun outch v -> v_to_string v |> String.print outch) strout m;
  BatIO.close_out strout
