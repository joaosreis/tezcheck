open Batteries

type ('a, 'b) t = ('a, 'b Set.t) Hashtbl.t

let is_empty = Hashtbl.is_empty

let add ht k v =
  let s = Hashtbl.find_default ht k Set.empty in
  Set.add v s |> Hashtbl.add ht k

let find = Hashtbl.find

let remove_all = Hashtbl.remove_all

let remove = Hashtbl.remove

let mem = Hashtbl.mem

let iter = Hashtbl.iter

let map = Hashtbl.map

let map_inplace = Hashtbl.map_inplace

let fold = Hashtbl.fold

let modify = Hashtbl.modify

let modify_def = Hashtbl.modify_def

let modify_opt = Hashtbl.modify_opt

let enum = Hashtbl.enum

let of_enum = Hashtbl.enum
