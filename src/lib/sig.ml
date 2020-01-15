module type Transfer = sig
  type vertex = Tezla_cfg.Cfg_node.t

  type state

  val initial_state : state

  val f : string -> vertex -> state -> state
end

module type Inter_transfer = sig
  include Transfer

  val f : string -> vertex -> state -> state

  val f1 : string -> vertex -> state -> state

  val f2 : string -> string -> vertex -> state -> state -> state
end

module type Lattice = sig
  include Fix.PROPERTY

  val lub : property -> property -> property

  val to_string : property -> string
end

module type Element = sig
  type t

  val to_string : t -> string
end

module type Call_context = sig
  type t

  type vertex = Tezla_cfg.Cfg_node.t

  val to_string : t -> string

  val initial_context : t

  val make_call_context : t -> int -> 'a -> int -> t
end
