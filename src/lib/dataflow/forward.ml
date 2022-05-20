module Make_solution
    (L : Lattices.Sig.S)
    (F : Transfer.S with type state = L.t) (P : sig
      val graph : Cfg.t
    end) =
struct
  module Solver = Solvers.Make_fix (L) (F)

  let solution = Solver.solution P.graph
  let result_to_string = L.to_string
end
