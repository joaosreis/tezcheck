open Base
open Utils

module Make (D : sig
  include Comparable.S

  val to_string : t -> string
end) =
struct
  type property = Set.M(D).t

  let bottom = Set.empty (module D)

  let lub = Set.union

  let equal = Set.equal

  let to_string x = Set.to_string ~fst:"[" ~lst:"]" ~sep:";" ~f:D.to_string x
end

module Make_reverse (D : sig
  include Comparable.S

  val to_string : t -> string
end) (B : sig
  val bottom : Set.M(D).t
end) =
struct
  type property = Set.M(D).t

  let bottom = B.bottom

  let lub = Set.inter

  let equal = Set.equal

  let to_string x = Set.to_string ~fst:"[" ~lst:"]" ~sep:";" ~f:D.to_string x
end
