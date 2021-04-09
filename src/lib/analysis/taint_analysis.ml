open Core_kernel
open Tezla_cfg
module Cfg = Flow_graph

let find_default m k default = match m k with None -> default | Some x -> x

let eval taint_map =
  let open Tezla.Adt in
  function
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
      find_default taint_map s Lattice.Taint.bottom
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
  | E_concat (s_1, s_2) -> (
      match
        ( find_default taint_map s_1 Lattice.Taint.bottom,
          find_default taint_map s_2 Lattice.Taint.bottom )
      with
      | Top, _ | _, Top -> Top
      | Element true, _ | _, Element true -> Element true
      | Bottom, _ | _, Bottom -> Bottom
      | _ -> Element false)
  | E_update (s_1, s_2, s_3)
  | E_slice (s_1, s_2, s_3)
  | E_check_signature (s_1, s_2, s_3)
  | E_operation (O_create_contract (_, s_1, s_2, s_3))
  | E_operation (O_transfer_tokens (s_1, s_2, s_3)) -> (
      match
        ( find_default taint_map s_1 Lattice.Taint.bottom,
          find_default taint_map s_2 Lattice.Taint.bottom,
          find_default taint_map s_3 Lattice.Taint.bottom )
      with
      | Top, _, _ | _, Top, _ | _, _, Top -> Top
      | Element true, _, _ | _, Element true, _ | _, _, Element true ->
          Element true
      | Bottom, _, _ | _, Bottom, _ | _, _, Bottom -> Bottom
      | _ -> Element false)
  | E_operation (O_create_account (s_1, s_2, s_3, s_4)) -> (
      match
        ( find_default taint_map s_1 Lattice.Taint.bottom,
          find_default taint_map s_2 Lattice.Taint.bottom,
          find_default taint_map s_3 Lattice.Taint.bottom,
          find_default taint_map s_4 Lattice.Taint.bottom )
      with
      | Top, _, _, _ | _, Top, _, _ | _, _, Top, _ | _, _, _, Top -> Top
      | Element true, _, _, _
      | _, Element true, _, _
      | _, _, Element true, _
      | _, _, _, Element true ->
          Element true
      | Bottom, _, _, _ | _, Bottom, _, _ | _, _, Bottom, _ | _, _, _, Bottom ->
          Bottom
      | _ -> Element false)
  | E_now -> Element true
  | E_balance | E_push _ | E_unit | E_none _ | E_self | E_amount | E_source
  | E_sender | E_create_contract_address _ | E_chain_id | E_lambda _ | E_nil _
  | E_contract_of_address (_, _)
  | E_special_empty_list _
  | E_special_empty_map (_, _)
  | E_empty_set _ | E_empty_map _ | E_empty_big_map _ | E_append _ ->
      Element false
  | E_apply (_, _) -> (*TODO:*) Element false

module RD_S = Reaching_definitions

module Solve (P : sig
  val graph : Cfg.t
end) =
struct
  module RD = RD_S.Solve (P)

  let blocks =
    let nodes_list = Cfg.nodes P.graph |> Sequence.to_list in
    Set.of_list (module Cfg_node) nodes_list

  let vars = RD_S.free_variables blocks

  module Var_tainting_lattice =
    Lattice.Map.Make
      (Tezla.Adt.Var)
      (struct
        let bottom_elems = vars
      end)
      (Lattice.Taint)

  module Reaching_definitions_lattice = Reaching_definitions.L
  module L =
    Lattice.Pair.Make (Reaching_definitions_lattice) (Var_tainting_lattice)

  let ta s n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, rv) ->
        let eval_rv = eval s rv in
        [ (lv, eval_rv) ]
    | _ -> []

  module F = struct
    type graph = Cfg.t

    type node = Cfg.G.Node.t

    type state = L.property

    let f _ n s =
      let g = RD.gen n in
      let k = RD.kill blocks n in
      let s1 = Set.diff (L.fst s) k |> Set.union g in
      let new_tv = ta (Var_tainting_lattice.get (L.snd s)) n in
      let s2 =
        List.fold_left new_tv ~init:(L.snd s) ~f:(fun m (i, eval) ->
            Var_tainting_lattice.set m i eval)
      in
      L.of_pair (s1, s2)

    let initial_state = L.bottom
  end

  include Dataflow.Forward.Make_solution (L) (F) (P)
end
