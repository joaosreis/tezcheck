open Batteries
include Hashtbl

let find_or_add t k v =
  try Hashtbl.find t k
  with Not_found ->
    let () = Hashtbl.add t k v in
    v
