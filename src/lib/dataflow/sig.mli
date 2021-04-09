module type Transfer = sig
  type graph = Tezla_cfg.Flow_graph.t

  type node = Tezla_cfg.Flow_graph.G.Node.t

  type state

  val initial_state : state

  val f : graph -> node -> state -> state
end

module type Inter_transfer = sig
  include Transfer

  val f : string -> node -> state -> state

  val f1 : string -> node -> state -> state

  val f2 : string -> string -> node -> state -> state -> state
end
