open Core_kernel
open Tezla_cfg
module Cfg = Flow_graph

module Definition_location = struct
  module T = struct
    type t = Cfg_node.var * int option [@@deriving ord, sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string (v, n) =
    let n' = match n with None -> "?" | Some n' -> Int.to_string n' in
    [%string "(%{v#Tezla.Adt.Var},%{n'})"]
end

type definition_location = Definition_location.t

let free_variables blocks =
  let free_variables_expr acc =
    let open Tezla.Adt in
    function
    | E_push _ -> acc
    | E_car s
    | E_cdr s
    | E_abs s
    | E_neg s
    | E_not s
    | E_eq s
    | E_neq s
    | E_lt s
    | E_gt s
    | E_leq s
    | E_geq s
    | E_left (s, _)
    | E_right (s, _)
    | E_some s
    | E_pack s
    | E_contract_of_address (_, s)
    | E_implicit_account s
    | E_blake2b s
    | E_sha256 s
    | E_sha512 s
    | E_hash_key s
    | E_address_of_contract s
    | E_unlift_option s
    | E_unlift_or_left s
    | E_unlift_or_right s
    | E_hd s
    | E_tl s
    | E_size s
    | E_isnat s
    | E_int_of_nat s
    | E_unpack (_, s)
    | E_dup s
    | E_var s
    | E_operation (O_set_delegate s)
    | E_concat_list s ->
        Set.add acc s
    | E_add (s_1, s_2)
    | E_sub (s_1, s_2)
    | E_mul (s_1, s_2)
    | E_div (s_1, s_2)
    | E_shiftL (s_1, s_2)
    | E_shiftR (s_1, s_2)
    | E_and (s_1, s_2)
    | E_or (s_1, s_2)
    | E_xor (s_1, s_2)
    | E_compare (s_1, s_2)
    | E_cons (s_1, s_2)
    | E_pair (s_1, s_2)
    | E_mem (s_1, s_2)
    | E_get (s_1, s_2)
    | E_exec (s_1, s_2)
    | E_concat (s_1, s_2)
    | E_append (s_1, s_2)
    | E_apply (s_1, s_2) ->
        let s' = Set.add acc s_1 in
        Set.add s' s_2
    | E_update (s_1, s_2, s_3)
    | E_slice (s_1, s_2, s_3)
    | E_check_signature (s_1, s_2, s_3)
    | E_operation (O_create_contract (_, s_1, s_2, s_3))
    | E_operation (O_transfer_tokens (s_1, s_2, s_3)) ->
        let s' = Set.add acc s_1 in
        let s' = Set.add s' s_2 in
        Set.add s' s_3
    | E_operation (O_create_account (s_1, s_2, s_3, s_4)) ->
        let s' = Set.add acc s_1 in
        let s' = Set.add s' s_2 in
        let s' = Set.add s' s_3 in
        Set.add s' s_4
    | E_unit | E_none _ | E_self | E_now | E_amount | E_balance | E_source
    | E_sender | E_create_contract_address _ | E_chain_id | E_lambda _ | E_nil _
    | E_empty_set _ | E_empty_map _ | E_empty_big_map _ | E_special_empty_list _
    | E_special_empty_map _ ->
        acc
  in
  let aux acc b =
    match b.Cfg_node.stmt with
    | Cfg_assign (_, e) -> free_variables_expr acc e
    | Cfg_failwith v
    | Cfg_if v
    | Cfg_if_cons v
    | Cfg_if_left v
    | Cfg_if_none v
    | Cfg_loop v
    | Cfg_loop_left v
    | Cfg_map v
    | Cfg_return v
    | Cfg_iter v ->
        Set.add acc v
    | Cfg_dig | Cfg_dug | Cfg_skip | Cfg_drop _ | Cfg_swap -> acc
  in
  Set.fold ~f:aux blocks ~init:Tezla.Adt.Var.Set.empty

let find_assignments blocks v =
  let aux acc b =
    let open Cfg_node in
    match b.stmt with
    | Cfg_assign (lv, _) when Tezla.Adt.Var.equal lv v -> Set.add acc b
    | Cfg_assign _ | Cfg_failwith _ | Cfg_if _ | Cfg_if_cons _ | Cfg_if_left _
    | Cfg_if_none _ | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _
    | Cfg_dig | Cfg_dug | Cfg_skip | Cfg_drop _ | Cfg_swap | Cfg_return _ ->
        acc
  in
  Set.fold ~f:aux blocks ~init:(Set.empty (module Cfg_node))

module L = Lattice.Powerset.Make (Definition_location)

module Solve (P : sig
  val graph : Flow_graph.t
end) =
struct
  let blocks =
    let nodes_list = Flow_graph.nodes P.graph |> Sequence.to_list in
    Set.of_list (module Cfg_node) nodes_list

  let kill blocks n =
    match n.Cfg_node.stmt with
    | Cfg_assign (lv, _) ->
        let assignments = find_assignments blocks lv in
        Set.fold assignments
          ~init:(Definition_location.Set.singleton (lv, None))
          ~f:(fun acc b -> Set.add acc (lv, Some b.Cfg_node.label))
    | _ -> Definition_location.Set.empty

  let gen n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, _) -> Definition_location.Set.singleton (lv, Some n.label)
    | _ -> Definition_location.Set.empty

  module F = struct
    type graph = Cfg.t

    type node = Flow_graph.G.Node.t

    type state = L.property

    let f _ n s =
      let g = gen n in
      let k = kill blocks n in
      Set.diff s k |> Set.union g

    let initial_state : Set.M(Definition_location).t =
      let fv = free_variables blocks in
      Set.fold fv
        ~init:(Set.empty (module Definition_location))
        ~f:(fun acc x -> Set.add acc (x, None))
  end

  include Dataflow.Forward.Make_solution (L) (F) (P)
end
