open Batteries
open Tezla_cfg
module Cfg = Flow_graph.Cfg

module Solve (P : sig
  val graph : Cfg.t
end) =
struct
  let data_sign_eval =
    let open Michelson.Adt in
    let open Sign_lattice in
    function D_int n -> sign n | _ -> Top

  let expr_sign_eval env =
    let get_val x = Map.find_default Sign_lattice.Bottom x env in
    let open Tezla.Adt in
    let open Sign_lattice in
    function
    | E_phi (s_1, s_2) -> Sign_lattice.lub (get_val s_1) (get_val s_2)
    (* TODO: check if it is correct *)
    | E_neg e -> (
        match get_val e with
        | Sign Pos -> Sign Neg
        | Sign Neg -> Sign Pos
        | s -> s )
    | E_int_of_nat e | E_dup e -> get_val e
    | E_abs e -> ( match get_val e with Sign Neg -> Sign Pos | v -> v )
    | E_add (e_1, e_2) -> plus (get_val e_1) (get_val e_2)
    | E_sub (e_1, e_2) -> minus (get_val e_1) (get_val e_2)
    | E_mul (e_1, e_2) -> times (get_val e_1) (get_val e_2)
    | E_div (e_1, e_2) -> divide (get_val e_1) (get_val e_2)
    | E_compare (e_1, e_2) -> (
        match (get_val e_1, get_val e_2) with
        | Bottom, _ | _, Bottom -> Bottom
        | Top, _ | _, Top -> Top
        | Sign Pos, (Sign Neg | Sign Zero) -> Sign Pos
        | Sign Neg, (Sign Pos | Sign Zero) -> Sign Neg
        | Sign Zero, Sign Zero -> Sign Zero
        | _ -> Top )
    | E_push (x, _) -> data_sign_eval x.d
    | E_shiftL _ | E_shiftR _ | E_car _ | E_cdr _ | E_and _ | E_or _ | E_xor _
    | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _ | E_cons _
    | E_operation _ | E_unit | E_pair _ | E_left _ | E_right _ | E_some _
    | E_none _ | E_mem _ | E_get _ | E_update _ | E_cast _ | E_concat _
    | E_slice _ | E_pack _ | E_unpack _ | E_self | E_contract_of_address _
    | E_implicit_account _ | E_now | E_amount | E_balance | E_check_signature _
    | E_blake2b _ | E_sha256 _ | E_sha512 _ | E_hash_key _ | E_steps_to_quota
    | E_source | E_sender | E_address_of_contract _
    | E_create_contract_address _ | E_unlift_option _ | E_unlift_or _ | E_hd _
    | E_tl _ | E_size _ | E_isnat _ | E_chain_id | E_create_account_address _
    | E_lambda _ | E_exec _ | E_nil _ | E_empty_set _ | E_empty_map _
    | E_empty_big_map _ | E_append _ | E_special_nil_list | E_mod _ | E_not _
    | E_concat_list _ ->
        Bottom

  let declaredVars =
    let blocks = Cfg.get_blocks P.graph in
    Hashtbl.fold
      (fun _ b acc ->
        let open Cfg_node in
        match b.stmt with Cfg_assign (lv, _) -> Set.add lv acc | _ -> acc)
      blocks Set.empty

  module L =
    Lattices.Map_lattice
      (struct
        type t = Cfg_node.ident

        let to_string = identity

        let bottom_elems = declaredVars
      end)
      (Sign_lattice)

  let sign_eval env n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, rv) -> [ (lv, expr_sign_eval env rv) ]
    | Cfg_if _ | Cfg_if_cons _ | Cfg_if_left _ | Cfg_if_none _ | Cfg_skip
    | Cfg_dig | Cfg_drop _ | Cfg_dug | Cfg_failwith _ | Cfg_swap | Cfg_loop _
    | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ ->
        []

  module F = struct
    type vertex = Cfg.vertex

    type state = L.property

    let f _ b s =
      let evals = sign_eval s b in
      List.fold_left (fun m (i, eval) -> L.set m i eval) s evals

    let initial_state =
      Set.fold
        (fun x map -> Map.add x Sign_lattice.Bottom map)
        declaredVars Map.empty
  end

  include Dataflow.Forward.Make_solution (L) (F) (P)
end
