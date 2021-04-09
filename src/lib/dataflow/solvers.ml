open Core_kernel

module Make_fix
    (L : Lattice.Sig.S)
    (F : Sig.Transfer with type state = L.property) =
struct
  module Cfg = Tezla_cfg.Flow_graph

  let solution ?(rev = false) (graph : Cfg.t) =
    let open Graphlib.Std in
    Graphlib.fixpoint
      (module Cfg.G)
      ~rev ~equal:L.equal ~merge:L.lub ~f:(F.f graph)
      ~init:(Solution.create (Map.empty (module Cfg.G.Node)) F.initial_state)
      graph.Cfg.graph
end
