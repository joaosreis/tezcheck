open Base

module Make (D : sig
  include Comparable.S

  val to_string : t -> string
end) : Sig.S with type property = Set.M(D).t

module Make_reverse (D : sig
  include Comparable.S

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) : Sig.S with type property = Set.M(D).t
