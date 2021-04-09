open Core_kernel
open Tezla_cfg
module Cfg = Flow_graph

module Solve (P : sig
  val graph : Cfg.t
end) =
struct
  let data_sign_eval =
    let open Tezla.Adt in
    let open Lattice.Sign in
    function
    | D_int n -> sig_bignum n
    | D_unit | D_none | D_string _ | D_bytes _ | D_bool _ | D_pair _ | D_left _
    | D_right _ | D_some _ | D_elt _ | D_list _ | D_instruction _ ->
        Top

  let expr_sign_eval env =
    let get_val x =
      match env x with None -> Lattice.Sign.bottom | Some x -> x
    in
    let open Tezla.Adt in
    let open Lattice.Sign in
    function
    | E_neg e -> (
        match get_val e with
        | Element Pos -> Lattice.Flat.Element Neg
        | Element Neg -> Element Pos
        | s -> s)
    | E_int_of_nat e | E_dup e | E_var e -> get_val e
    | E_abs e -> ( match get_val e with Element Neg -> Element Pos | v -> v)
    | E_add (e_1, e_2) -> plus (get_val e_1) (get_val e_2)
    | E_sub (e_1, e_2) -> minus (get_val e_1) (get_val e_2)
    | E_mul (e_1, e_2) -> times (get_val e_1) (get_val e_2)
    | E_div (e_1, e_2) -> divide (get_val e_1) (get_val e_2)
    | E_compare (e_1, e_2) -> (
        match (get_val e_1, get_val e_2) with
        | Bottom, _ | _, Bottom -> Bottom
        | Top, _ | _, Top -> Top
        | Element Pos, (Element Neg | Element Zero) -> Element Pos
        | Element Neg, (Element Pos | Element Zero) -> Element Neg
        | Element Zero, Element Zero -> Element Zero
        | _ -> Top)
    | E_push (x, _) -> data_sign_eval x
    | E_shiftL _ | E_shiftR _ | E_car _ | E_cdr _ | E_and _ | E_or _ | E_xor _
    | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _ | E_cons _
    | E_operation _ | E_unit | E_pair _ | E_left _ | E_right _ | E_some _
    | E_none _ | E_mem _ | E_get _ | E_update _ | E_concat _ | E_slice _
    | E_pack _ | E_unpack _ | E_self | E_contract_of_address _
    | E_implicit_account _ | E_now | E_amount | E_balance | E_check_signature _
    | E_blake2b _ | E_sha256 _ | E_sha512 _ | E_hash_key _ | E_source | E_sender
    | E_address_of_contract _ | E_create_contract_address _ | E_unlift_option _
    | E_unlift_or_left _ | E_unlift_or_right _ | E_hd _ | E_tl _ | E_size _
    | E_isnat _ | E_chain_id | E_lambda _ | E_exec _ | E_nil _ | E_empty_set _
    | E_empty_map _ | E_empty_big_map _ | E_append _ | E_not _ | E_concat_list _
    | E_special_empty_list _ | E_special_empty_map _ ->
        Bottom
    | E_apply _ -> (*TODO:*) Bottom

  let declaredVars =
    let blocks = Cfg.nodes P.graph in
    Sequence.fold
      ~f:(fun acc b ->
        match b.Cfg_node.stmt with
        | Cfg_assign (lv, _) -> Set.add acc lv
        | _ -> acc)
      ~init:(Set.empty (module Tezla.Adt.Var))
      blocks

  module L =
    Lattice.Map.Make
      (Tezla.Adt.Var)
      (struct
        let bottom_elems = declaredVars
      end)
      (Lattice.Sign)

  let sign_eval env n =
    let open Cfg_node in
    match n.stmt with
    | Cfg_assign (lv, rv) -> [ (lv, expr_sign_eval env rv) ]
    | Cfg_if _ | Cfg_if_cons _ | Cfg_if_left _ | Cfg_if_none _ | Cfg_skip
    | Cfg_dig | Cfg_drop _ | Cfg_dug | Cfg_failwith _ | Cfg_swap | Cfg_loop _
    | Cfg_loop_left _ | Cfg_map _ | Cfg_iter _ | Cfg_return _ ->
        []

  module F = struct
    type graph = Cfg.t

    type node = Cfg.G.Node.t

    type state = L.property

    let f _ n s =
      let evals = sign_eval (L.get s) n in
      List.fold_left evals ~init:s ~f:(fun m (i, eval) -> L.set m i eval)

    let initial_state = L.bottom
  end

  include Dataflow.Forward.Make_solution (L) (F) (P)
end
