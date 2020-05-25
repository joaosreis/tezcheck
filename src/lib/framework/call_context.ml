(* TODO other contexts *)

module Functional_context (L : Sig.Lattice) = struct
  type t = L.property

  type label = int

  type l_property = L.property

  let to_string = L.to_string

  let initial_context = L.bottom

  let make_call_context _ _ x _ = x
end
