type 'a t = Top | Some of 'a

module Make (L : Sig.S) : Sig.S with type property = L.property t
