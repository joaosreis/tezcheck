type 'a t = Bottom | Top | Element of 'a

module Make (X : Sig.ELEMENT) : Sig.S with type property = X.t t
