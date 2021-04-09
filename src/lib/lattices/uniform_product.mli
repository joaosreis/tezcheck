module Make
  (L : Sig.S) (N : sig
    val n : int
  end) : Sig.S with type property = L.property list
