open Base

module Make (D : sig
  include Comparable.S

  val to_string : t -> string
end) (B : sig
  val bottom_elems : Set.M(D).t
end)
(L : Sig.S) : sig
  include Sig.S

  val set : property -> D.t -> L.property -> property

  val get : property -> D.t -> L.property option
end
