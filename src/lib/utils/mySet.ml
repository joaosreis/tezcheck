open Batteries
include Set

(* HACK inefficient *)
let find_prep prep s = List.find prep (to_list s)
