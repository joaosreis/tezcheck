open! Core
open Graphlib.Std

module Make (D : Domains.Domain.DOMAIN) = struct
  let solution graph param storage =
    let module Cfg = Tezla_cfg.Flow_graph in
    let env' = D.init_env graph.Cfg.vars in
    let f n s =
      let open Tezla_cfg.Cfg_node in
      match n.stmt with
      | Cfg_skip -> s
      | Cfg_assign (v, e) -> D.assign s v e
      (* | CFG_guard b | CFG_assert b -> D.guard abs_src b *)
      (* | CFG_call _ -> TODO: abs_src *)
      | _ -> s
    in
    let start = graph.Cfg.initial in
    Graphlib.fixpoint
      (module Cfg.G)
      ~start
      ~step:(fun i _ s s' -> if i > 3 then D.widen s s' else s')
      ~init:
        (Solution.create
           (Map.empty (module Cfg.G.Node))
           (D.init env' param storage))
      ~equal:D.equals ~merge:D.join ~f graph.Cfg.graph
end
