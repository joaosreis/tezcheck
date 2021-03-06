open! Core

module type VALUE_DOMAIN = sig
  (* type of abstract elements *)
  (* an element of type t abstracts a set of integers *)
  type t [@@deriving sexp_of]

  (* unrestricted value: [-oo,+oo] *)
  val top : t

  (* bottom value: empty set *)
  val bottom : t

  (* abstract value for a type *)
  val type_abs : Tezla.Adt.ttyp -> t

  (* abstract value for (parameter, storage) *)
  val param_storage_abs : Tezla.Adt.ttyp -> Tezla.Adt.ttyp -> t

  (* set-theoretic operations *)
  val join : t -> t -> t
  val meet : t -> t -> t

  (* widening *)
  val widen : t -> t -> t

  (* subset inclusion of concretizations *)
  val leq : t -> t -> bool

  (* check the emptiness of the concretization *)
  val is_bottom : t -> bool

  (* abstract an expression *)
  val expr : (Tezla.Adt.var -> t option) -> Tezla.Adt.expr -> t

  (* abstract element to string *)
  val to_string : t -> string
end
