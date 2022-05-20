module type S = sig
  include Transfer.S

  val f : string -> node -> state -> state

  val f1 : string -> node -> state -> state

  val f2 : string -> string -> node -> state -> state -> state
end
