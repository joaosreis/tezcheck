open! Core
open Tezla_cfg

module Make (D : Value_domain.VALUE_DOMAIN) = struct
  type t = D.t Cfg_node.Var.Map.t
  type env = Cfg_node.Var.Set.t

  let init_env x = x

  let init env param storage =
    let a = Map.empty (module Cfg_node.Var) in
    let v =
      Cfg_node.Var.
        {
          var_name = "parameter_storage";
          var_type = (Pair (param, storage), []);
        }
    in
    let a = Map.set a ~key:v ~data:(D.param_storage_abs param storage) in
    Cfg_node.Var.Set.fold env ~init:a ~f:(fun acc key ->
        Map.set acc ~key ~data:D.bottom)

  let bottom env =
    Cfg_node.Var.Set.fold env
      ~f:(fun acc v -> Map.set acc ~key:v ~data:D.bottom)
      ~init:(Map.empty (module Cfg_node.Var))

  let assign a v e = Map.set a ~key:v ~data:(D.expr (Map.find a) e)

  let join x y =
    Map.fold x ~init:y ~f:(fun ~key ~data:a acc ->
        match Map.find acc key with
        | None -> Map.set acc ~key ~data:a
        | Some a' -> Map.set acc ~key ~data:(D.join a a'))

  let widen x y =
    Map.mapi y ~f:(fun ~key:v ~data:a ->
        match Map.find x v with None -> a | Some a' -> D.widen a' a)

  let leq x y =
    Map.for_alli x ~f:(fun ~key:v ~data:a ->
        match Map.find y v with None -> false | Some a' -> D.leq a a')

  let equals x y =
    Map.length x = Map.length y
    && Map.for_alli x ~f:(fun ~key:v ~data:a ->
           match Map.find y v with
           | None -> false
           | Some a' -> D.leq a a' && D.leq a' a)

  let is_bottom = Map.is_empty

  let to_string a =
    let l = Map.to_alist a in
    let open Utils in
    List.to_string ~fst:"[" ~lst:"]" ~sep:"; "
      ~f:(fun (k, v) -> [%string "%{k#Cfg_node.Var}: %{v#D}"])
      l

  let env x = Cfg_node.Var.Set.of_list (Map.keys x)

  let environment_to_string a =
    let l = Cfg_node.Var.Set.to_list (env a) in
    let open Utils in
    List.to_string ~fst:"[" ~lst:"]" ~sep:"; " ~f:Cfg_node.Var.to_string l
end
