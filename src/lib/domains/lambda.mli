type lambda = Lambda_data of int | Lambda_expr of int [@@deriving ord, sexp]

module P_set : Lattices.Powerset.S with type Elt.t = lambda

type t = [ `Some of P_set.t | `Top ]

include Value_domain.VALUE_DOMAIN with type t := t
