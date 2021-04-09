module Make (L1 : Sig.S) (L2 : Sig.S) : sig
  include Sig.S

  val fst : property -> L1.property

  val snd : property -> L2.property

  val of_pair : L1.property * L2.property -> property
end
