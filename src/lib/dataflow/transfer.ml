module type S = sig
  type graph = Tezla_cfg.Flow_graph.t
  type node = Tezla_cfg.Flow_graph.G.Node.t
  type state

  val initial_state : state
  val f : graph -> node -> state -> state
end
