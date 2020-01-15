open Batteries
open Fix

module Imperative_map (K : sig
  type key
end) =
struct
  type key = K.key

  type 'data t = (key, 'data) Hashtbl.t

  let create () = Hashtbl.create 10

  let clear ht = Hashtbl.clear ht

  let add k v ht = Hashtbl.replace ht k v

  let find k ht = Hashtbl.find ht k

  let iter f ht = Hashtbl.iter f ht
end

module Make_fix
    (L : Sig.Lattice)
    (F : Sig.Transfer with type state = L.property)
    (D : Dependencies.S) =
struct
  module Cfg = Tezla_cfg.Flow_graph.Cfg

  type variable = Circ of int | Bullet of int

  module Fix =
    Fix.ForHashedType
      (struct
        type t = variable

        let hash = Hashtbl.hash

        let equal a b =
          match (a, b) with
          | Circ x, Circ y | Bullet x, Bullet y -> x = y
          | _ -> false
      end)
      (L)

  let generate_equations graph var state =
    match var with
    | Circ l ->
        D.indep graph l
        |> List.map (fun l' -> state (Bullet l'))
        |> List.fold_left L.lub
             (if D.is_extremal graph l then F.initial_state else L.bottom)
    | Bullet l ->
        F.f (Cfg.get_func_id graph l) (Cfg.get graph l) (state (Circ l))

  let solve graph = Fix.lfp (generate_equations graph)
end

(* module Make_fix_inter
    (N : Cfg_node.S)
    (Cfg : Sig.Inter_flow_graph
             with type vertex = N.stmt N.t
              and type expr = N.expr)
    (L : Sig.Lattice)
    (F : Sig.Inter_transfer
           with type vertex = Cfg.vertex
            and type state = L.property)
    (D : Dependencies.S with type g_t = Cfg.t) =
struct
  type variable = Circ of int | Bullet of int

  module Fix =
    Fix.ForHashedType
      (struct
        type t = variable

        let hash = Hashtbl.hash

        let equal a b =
          match (a, b) with
          | Circ x, Circ y | Bullet x, Bullet y -> x = y
          | _ -> false
      end)
      (L)

  let generate_equations graph var state =
    match var with
    | Circ l ->
        D.indep graph l
        |> List.map (fun l' -> state (Bullet l'))
        |> List.fold_left L.lub
             (if D.is_extremal graph l then F.initial_state else L.bottom)
    | Bullet l ->
        let v = Cfg.get graph l in
        let fid = Cfg.get_func_id graph l in
        if List.exists (fun (lc, _, _, _) -> lc = l) (Cfg.inter_flow graph) then
          F.f1 (Cfg.get_func_id graph l) v (state (Circ l))
        else if List.exists (fun (_, _, _, lr) -> lr = l) (Cfg.inter_flow graph)
        then
          Cfg.inter_flow graph
          |> List.filter_map (fun (lc, _, _, lr) ->
                 if lr = l then Some (Cfg.get_func_id graph lc, lc) else None)
          |> List.fold_left
               (fun acc (fid2, lc) ->
                 F.f2 fid fid2 v (state (Circ lc)) (state (Circ l)) |> L.lub acc)
               L.bottom
        else F.f (Cfg.get_func_id graph l) v (state (Circ l))

  let solve graph = generate_equations graph |> Fix.lfp
end *)
