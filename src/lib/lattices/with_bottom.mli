type 'a t = Bottom | Some of 'a

module Make (L : Sig.S) : Sig.S with type property = L.property t
