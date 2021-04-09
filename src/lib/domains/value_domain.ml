(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of integers
  (for instance: constants or intervals).
 *)

module type VALUE_DOMAIN = sig
  (* type of abstract elements *)
  (* an element of type t abstracts a set of integers *)
  type t

  (* unrestricted value: [-oo,+oo] *)
  val top : t

  (* bottom value: empty set *)
  val bottom : t

  (* abstract value for a type *)
  val type_abs : Tezla.Adt.typ -> t

  (* abstract value for (parameter, storage) *)
  val param_storage_abs : Tezla.Adt.typ -> Tezla.Adt.typ -> t

  (* set-theoretic operations *)
  val join : t -> t -> t

  val meet : t -> t -> t

  (* widening *)
  val widen : t -> t -> t

  (* subset inclusion of concretizations *)
  val subset : t -> t -> bool

  (* check the emptiness of the concretization *)
  val is_bottom : t -> bool

  (* abstract an expression *)
  val expr : (Tezla.Adt.var -> t option) -> Tezla.Adt.expr -> t

  (* abstract element to string *)
  val to_string : t -> string
end
