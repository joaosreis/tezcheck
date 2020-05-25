include BatHashtbl

let create = create

let find ht x = find_default ht x BatSet.empty

let add ht x v =
  let s = find ht x in
  Hashtbl.add ht x (BatSet.add v s)
