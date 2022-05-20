open! Core

module Make_fix (L : Lattices.Sig.S) (F : Transfer.S with type state = L.t) =
struct
  let solution ?(rev = false) (graph : Cfg.t) =
    let open Graphlib.Std in
    Graphlib.fixpoint
      (module Cfg.G)
      ~rev ~equal:L.leq ~merge:L.join ~f:(F.f graph)
      ~init:(Solution.create (Map.empty (module Cfg.G.Node)) F.initial_state)
      graph.Cfg.graph
end
