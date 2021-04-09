module Cfg = Tezla_cfg.Flow_graph

module Forward = struct
  module Make_solution
      (L : Lattice.Sig.S)
      (F : Sig.Transfer with type state = L.property) (P : sig
        val graph : Cfg.t
      end) =
  struct
    module Solver = Solvers.Make_fix (L) (F)

    let solution = Solver.solution P.graph

    let result_to_string = L.to_string
  end
end

module Backward = struct
  module Make_solution
      (L : Lattice.Sig.S)
      (F : Sig.Transfer with type state = L.property) (P : sig
        val graph : Cfg.t
      end) =
  struct
    module Solver = Solvers.Make_fix (L) (F)

    let solution = Solver.solution ~rev:true P.graph

    let result_to_string = L.to_string
  end
end
