open Batteries
open Tezla_cfg
module Cfg = Flow_graph.Cfg

type definition_location = string * Cfg.vertex option

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
    | E_cast s
    | E_pack s
    | E_contract_of_address s
    | E_implicit_account s
    | E_blake2b s
    | E_sha256 s
    | E_sha512 s
    | E_hash_key s
    | E_address_of_contract s
    | E_unlift_option s
    | E_unlift_or s
    | E_hd s
    | E_tl s
    | E_size s
    | E_isnat s
    | E_int_of_nat s
    | E_unpack (_, s)
    | E_dup s
    | E_operation (O_set_delegate s)
    | E_concat_list s ->
        Set.add s acc
    | E_phi (s_1, s_2)
    | E_mod (s_1, s_2)
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
    | E_append (s_1, s_2) ->
        Set.add s_1 acc |> Set.add s_2
    | E_update (s_1, s_2, s_3)
    | E_slice (s_1, s_2, s_3)
    | E_check_signature (s_1, s_2, s_3)
    | E_operation (O_create_contract (_, s_1, s_2, s_3))
    | E_operation (O_transfer_tokens (s_1, s_2, s_3)) ->
        Set.add s_1 acc |> Set.add s_2 |> Set.add s_3
    | E_operation (O_create_account (s_1, s_2, s_3, s_4)) ->
        Set.add s_1 acc |> Set.add s_2 |> Set.add s_3 |> Set.add s_4
    | E_unit | E_none _ | E_self | E_now | E_amount | E_balance
    | E_steps_to_quota | E_source | E_sender | E_create_contract_address _
    | E_chain_id | E_create_account_address _ | E_lambda _ | E_nil _
    | E_empty_set _ | E_empty_map _ | E_empty_big_map _ | E_special_nil_list ->
        acc
  in
  let open Cfg_node in
  let aux b acc =
    match b.stmt with
    | Cfg_assign (_, e) -> free_variables_expr acc e
    | Cfg_failwith v
    | Cfg_if v
    | Cfg_if_cons v
    | Cfg_if_left v
    | Cfg_if_none v
    | Cfg_loop v
    | Cfg_loop_left v
    | Cfg_map v
    | Cfg_iter v ->
        Set.add v acc
    | Cfg_dig | Cfg_dug | Cfg_skip | Cfg_drop _ | Cfg_swap -> acc
  in
  Set.fold aux blocks Set.empty

let find_assignments blocks v =
  let aux b acc =
    let open Cfg_node in
    match b.stmt with
    | Cfg_assign (lv, _) when lv = v -> Set.add b acc
    | Cfg_assign _ | Cfg_failwith _ | Cfg_if _ | Cfg_if_cons _ | Cfg_if_left _
    | Cfg_if_none _ | Cfg_loop _ | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _
    | Cfg_dig | Cfg_dug | Cfg_skip | Cfg_drop _ | Cfg_swap ->
        acc
  in
  Set.fold aux blocks Set.empty

(*module Make
    (N : Cfg_node.S)
    (Cfg : Sig.Flow_graph with type vertex = N.stmt N.t)
struct*)
module Solve (P : sig
  val graph : Flow_graph.Cfg.t
end) =
struct
  let blocks =
    Hashtbl.fold
      (fun _ -> Set.add)
      (Flow_graph.Cfg.get_blocks P.graph)
      Set.empty

  module L = Lattices.Powerset_lattice (struct
    type t = definition_location

    let to_string (v, n) =
      Printf.sprintf "(%s,%s)" v
        (match n with None -> "?" | Some n' -> string_of_int n'.Cfg_node.id)
  end)

  let kill blocks n =
    let pair x y = (x, y) in
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, _) ->
        Set.map (pair lv % Option.some) (find_assignments blocks lv)
        |> Set.add (lv, None)
    | _ -> Set.empty

  let gen n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, _) -> Set.singleton (lv, Some n)
    | _ -> Set.empty

  module F = struct
    type vertex = Cfg.vertex

    type state = L.property

    let f _ n s =
      let g = gen n in
      let k = kill blocks n in
      Set.diff s k |> Set.union g

    let initial_state = Set.map (fun x -> (x, None)) (free_variables blocks)
  end

  module Fix = Solvers.Make_fix (L) (F) (Dependencies.Forward)

  let solution = Fix.solve P.graph

  let get_entry_result l = solution (Fix.Circ l)

  let get_exit_result l = solution (Fix.Bullet l)

  let result_to_string = L.to_string
end
