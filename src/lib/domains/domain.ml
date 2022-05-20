(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Antoine Miné 2015
  Marc Chevalier 2018
  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of envrionments
  (for instance: a map from variable to their bounds).
 *)

module type DOMAIN = sig
  (* type of abstract elements *)
  (* an element of type t abstracts a set of mappings from variables
     to integers
  *)
  type t

  (* type of environments binding dimensions to names *)
  type env

  (* initial environment *)
  val init_env : Tezla_cfg.Cfg_node.Var.Set.t -> env

  (* initial abstract element *)
  val init : env -> Tezla.Adt.ttyp -> Tezla.Adt.ttyp -> t

  (* empty set of environments *)
  val bottom : env -> t

  (* assign *)
  val assign : t -> Tezla.Adt.var -> Tezla.Adt.expr -> t

  (* abstract join *)
  val join : t -> t -> t

  (* widening *)
  val widen : t -> t -> t

  (* whether an abstract element is included in another one *)
  val leq : t -> t -> bool

  (* whether an abstract element is equal to another *)
  val equals : t -> t -> bool

  (* whether the abstract element represents the empty set *)
  val is_bottom : t -> bool

  (* to string *)
  val to_string : t -> string

  (* get environment from abstract element *)
  val env : t -> env
  val environment_to_string : t -> string
end
