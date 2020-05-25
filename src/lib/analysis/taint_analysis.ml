open Batteries
open Tezla_cfg
module Cfg = Flow_graph.Cfg

let eval taint_map =
  let open Tezla.Adt in
  let open Taint_lattice in
  function
  | E_phi (s_1, s_2) ->
      lub
        (Map.find_default Taint_lattice.Bottom s_1 taint_map)
        (Map.find_default Taint_lattice.Bottom s_2 taint_map)
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
      Map.find_default Taint_lattice.Bottom s taint_map
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
  | E_mod (s_1, s_2) -> (
      match
        ( Map.find_default Taint_lattice.Bottom s_1 taint_map,
          Map.find_default Taint_lattice.Bottom s_2 taint_map )
      with
      | Top, _ | _, Top -> Top
      | Element true, _ | _, Element true -> Element true
      | Bottom, _ | _, Bottom -> Bottom
      | _ -> Element false )
  | E_update (s_1, s_2, s_3)
  | E_slice (s_1, s_2, s_3)
  | E_check_signature (s_1, s_2, s_3)
  | E_operation (O_create_contract (_, s_1, s_2, s_3))
  | E_operation (O_transfer_tokens (s_1, s_2, s_3)) -> (
      match
        ( Map.find_default Taint_lattice.Bottom s_1 taint_map,
          Map.find_default Taint_lattice.Bottom s_2 taint_map,
          Map.find_default Taint_lattice.Bottom s_3 taint_map )
      with
      | Top, _, _ | _, Top, _ | _, _, Top -> Top
      | Element true, _, _ | _, Element true, _ | _, _, Element true ->
          Element true
      | Bottom, _, _ | _, Bottom, _ | _, _, Bottom -> Bottom
      | _ -> Element false )
  | E_operation (O_create_account (s_1, s_2, s_3, s_4)) -> (
      match
        ( Map.find_default Taint_lattice.Bottom s_1 taint_map,
          Map.find_default Taint_lattice.Bottom s_2 taint_map,
          Map.find_default Taint_lattice.Bottom s_3 taint_map,
          Map.find_default Taint_lattice.Bottom s_4 taint_map )
      with
      | Top, _, _, _ | _, Top, _, _ | _, _, Top, _ | _, _, _, Top -> Top
      | Element true, _, _, _
      | _, Element true, _, _
      | _, _, Element true, _
      | _, _, _, Element true ->
          Element true
      | Bottom, _, _, _ | _, Bottom, _, _ | _, _, Bottom, _ | _, _, _, Bottom ->
          Bottom
      | _ -> Element false )
  | E_now -> Element true
  | E_balance | E_push _ | E_unit | E_none _ | E_self | E_amount
  | E_steps_to_quota | E_source | E_sender | E_create_contract_address _
  | E_chain_id | E_create_account_address _ | E_lambda _ | E_nil _
  | E_empty_set _ | E_empty_map _ | E_empty_big_map _ | E_append _
  | E_special_nil_list ->
      Element false

module RD_S = Reaching_definitions

module Solve (P : sig
  val graph : Cfg.t
end) =
struct
  module RD = RD_S.Solve (P)

  let blocks =
    Hashtbl.fold (fun _ -> Set.add) (Cfg.get_blocks P.graph) Set.empty

  let vars = RD_S.free_variables blocks

  module Var_tainting_lattice =
    Lattices.Map_lattice
      (struct
        type t = string

        let to_string = identity

        let bottom_elems = vars
      end)
      (Taint_lattice)

  module Reaching_definitions_lattice = Lattices.Powerset_lattice (struct
    type t = RD_S.definition_location

    let to_string (v, n) =
      Printf.sprintf "(%s,%s)" v
        (match n with None -> "?" | Some n' -> string_of_int n'.Cfg_node.id)
  end)

  module L =
    Lattices.Pair_lattice (Reaching_definitions_lattice) (Var_tainting_lattice)

  let ta s n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, rv) ->
        let eval_rv = eval s rv in
        [ (lv, eval_rv) ]
    | _ -> []

  module F = struct
    type vertex = Cfg.vertex

    type state = L.property

    let f _ b s =
      let g = RD.gen b in
      let k = RD.kill blocks b in
      let s1 = Set.diff (fst s) k |> Set.union g in
      let new_tv = ta (snd s) b in
      let s2 =
        List.fold_left
          (fun m (i, eval) -> Var_tainting_lattice.set m i eval)
          (snd s) new_tv
      in
      (s1, s2)

    let initial_state =
      ( Set.map (fun x -> (x, None)) vars,
        Set.fold
          (fun x acc -> Map.add x Taint_lattice.bottom acc)
          vars Map.empty )
  end

  include Dataflow.Forward.Make_solution (L) (F) (P)
end
