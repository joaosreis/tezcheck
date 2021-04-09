module type S = sig
  type property

  val bottom : property

  val equal : property -> property -> bool

  val lub : property -> property -> property

  val to_string : property -> string
end

module type ELEMENT = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end
