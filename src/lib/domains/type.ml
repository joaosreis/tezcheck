module Make (D : Value_domain.VALUE_DOMAIN) = struct
  type t =
    | Single of D.t
    | Pair of t * t
    | Left of t
    | Right of t
    | Or of t * t
    | Some of t
    | None
    | Option of t
    | Top
    | Bottom

  let top = Top

  let bottom = Bottom

  let rec type_abs =
    let open Tezla.Adt.Typ in
    function
    | T_pair (t_1, t_2) -> Pair (type_abs t_1, type_abs t_2)
    | T_or (t_1, t_2) -> Or (type_abs t_1, type_abs t_2)
    | T_option t -> Option (type_abs t)
    | ( T_key | T_unit | T_signature | T_operation | T_chain_id | T_int | T_nat
      | T_string | T_bytes | T_mutez | T_bool | T_key_hash | T_timestamp
      | T_address | T_list _ | T_set _ | T_contract _
      | T_lambda (_, _)
      | T_map (_, _)
      | T_big_map (_, _) ) as t ->
        Single (D.type_abs t)

  let param_storage_abs param storage = Pair (type_abs param, type_abs storage)

  let rec join x y =
    match (x, y) with
    | Single x, Single y -> Single (D.join x y)
    | Pair (x_a, x_b), Pair (y_a, y_b) -> Pair (join x_a y_a, join x_b y_b)
    | Left x, Left y -> Left (join x y)
    | Right x, Right y -> Right (join x y)
    | Left l, Right r | Right r, Left l -> Or (l, r)
    | Or (x_l, x_r), Or (y_l, y_r) -> Or (join x_l y_l, join x_r y_r)
    | Left l, Or (x_l, x_r) | Or (x_l, x_r), Left l -> Or (join l x_l, x_r)
    | Right r, Or (x_l, x_r) | Or (x_l, x_r), Right r -> Or (x_l, join r x_r)
    | Some x, Some y -> Some (join x y)
    | None, None -> None
    | Some x, None | None, Some x -> Option x
    | Option x, Option y -> Option (join x y)
    | Some x, Option y | Option x, Some y -> Option (join x y)
    | Top, _ | _, Top -> Top
    | Bottom, x | x, Bottom -> x
    | _ ->
        Core_kernel.Debug.amf [%here] "invalid types";
        assert false

  let rec meet x y =
    match (x, y) with
    | Single x, Single y -> Single (D.meet x y)
    | Pair (x_a, x_b), Pair (y_a, y_b) -> Pair (meet x_a y_a, meet x_b y_b)
    | Left x, Left y -> Left (meet x y)
    | Right x, Right y -> Right (meet x y)
    | Left _, Right _ | Right _, Left _ -> Bottom
    | Or (x_l, x_r), Or (y_l, y_r) -> Or (meet x_l y_l, meet x_r y_r)
    | Left l, Or (x_l, _) | Or (x_l, _), Left l -> Left (meet l x_l)
    | Right r, Or (_, x_r) | Or (_, x_r), Right r -> Right (meet r x_r)
    | Some x, Some y -> Some (meet x y)
    | None, None -> None
    | Some _, None | None, Some _ -> Bottom
    | Option x, Option y -> Option (meet x y)
    | Some x, Option y | Option x, Some y -> Some (meet x y)
    | Top, x | x, Top -> x
    | Bottom, _ | _, Bottom -> Bottom
    | _ ->
        Core_kernel.Debug.amf [%here] "invalid types";
        assert false

  let rec widen x y =
    match (x, y) with
    | Single x, Single y -> Single (D.widen x y)
    | Pair (x_a, x_b), Pair (y_a, y_b) -> Pair (widen x_a y_a, widen x_b y_b)
    | Left x, Left y -> Left (widen x y)
    | Right x, Right y -> Right (widen x y)
    | Left l, Right r | Right r, Left l -> Or (l, r)
    | Or (x_l, x_r), Or (y_l, y_r) -> Or (widen x_l y_l, widen x_r y_r)
    | Left l, Or (x_l, x_r) -> Or (widen l x_l, x_r)
    | Or (x_l, _), Left l -> Left (widen x_l l)
    | Right r, Or (x_l, x_r) -> Or (x_l, widen r x_r)
    | Or (_, x_r), Right r -> Right (widen r x_r)
    | Some x, Some y -> Some (widen x y)
    | None, None -> None
    | Some _, None -> None
    | None, Some x -> Some x
    | Option x, Option y -> Option (widen x y)
    | None, Option x -> Option x
    | Option _, None -> None
    | Some x, Option y -> Option (widen x y)
    | Option x, Some y -> Some (widen x y)
    | Top, x -> x
    | _, Top -> Top
    | Bottom, x -> x
    | _, Bottom -> Bottom
    | _ ->
        Core_kernel.Debug.amf [%here] "invalid types";
        assert false

  let rec subset x y =
    match (x, y) with
    | Single x, Single y -> D.subset x y
    | Left x, Left y | Right x, Right y | Some x, Some y | Option x, Option y ->
        subset x y
    | Pair (x_a, x_b), Pair (y_a, y_b) | Or (x_a, x_b), Or (y_a, y_b) ->
        subset x_a y_a && subset x_b y_b
    | (Left _ | Right _), Or _ -> true
    | Or _, (Left _ | Right _) -> false
    | (Some _ | None), Option _ -> true
    | Option _, (Some _ | None) -> false
    | Some _, None | None, Some _ -> false
    | None, None -> true
    | Top, _ -> false
    | _, Top -> true
    | Bottom, _ -> true
    | _, Bottom -> false
    | Left _, Right _ | Right _, Left _ -> false
    | _ ->
        Core_kernel.Debug.amf [%here] "invalid types";
        assert false

  let is_bottom = function Bottom -> true | _ -> false

  let expr abs_var =
    let open Tezla.Adt.Var in
    let abs_var_or_not_found l v =
      match abs_var v with
      | Option.Some x -> x
      | None ->
          Core_kernel.Debug.amf l "var not found: %s\n" v.var_name;
          assert false
    in
    let invalid_type l =
      Core_kernel.Debug.amf l "invalid type";
      assert false
    in
    let open Tezla.Adt in
    function
    | E_car v -> (
        match abs_var_or_not_found [%here] v with
        | Pair (x, _) -> x
        | _ -> invalid_type [%here])
    | E_cdr v -> (
        match abs_var_or_not_found [%here] v with
        | Pair (_, x) -> x
        | _ -> invalid_type [%here])
    | E_pair (v_1, v_2) ->
        Pair (abs_var_or_not_found [%here] v_1, abs_var_or_not_found [%here] v_2)
    | E_left (v, _) -> Left (abs_var_or_not_found [%here] v)
    | E_right (v, _) -> Right (abs_var_or_not_found [%here] v)
    | E_unlift_or_left v -> (
        match abs_var_or_not_found [%here] v with
        | Left x | Or (x, _) -> x
        | _ -> invalid_type [%here])
    | E_unlift_or_right v -> (
        match abs_var_or_not_found [%here] v with
        | Right x | Or (_, x) -> x
        | _ -> invalid_type [%here])
    | E_some v -> Some (abs_var_or_not_found [%here] v)
    | E_none _ -> None
    | E_unlift_option v -> (
        match abs_var_or_not_found [%here] v with
        | Some x | Option x -> x
        | _ -> invalid_type [%here])
    | E_var v | E_dup v -> abs_var_or_not_found [%here] v
    | ( E_unit | E_self | E_now | E_amount | E_balance | E_source | E_sender
      | E_chain_id
      | E_push (_, _)
      | E_abs _ | E_neg _ | E_not _
      | E_add (_, _)
      | E_sub (_, _)
      | E_mul (_, _)
      | E_div (_, _)
      | E_shiftL (_, _)
      | E_shiftR (_, _)
      | E_and (_, _)
      | E_or (_, _)
      | E_xor (_, _)
      | E_eq _ | E_neq _ | E_lt _ | E_gt _ | E_leq _ | E_geq _
      | E_compare (_, _)
      | E_cons (_, _)
      | E_operation _
      | E_mem (_, _)
      | E_get (_, _)
      | E_update (_, _, _)
      | E_concat (_, _)
      | E_concat_list _
      | E_slice (_, _, _)
      | E_pack _
      | E_unpack (_, _)
      | E_contract_of_address (_, _)
      | E_implicit_account _
      | E_check_signature (_, _, _)
      | E_blake2b _ | E_sha256 _ | E_sha512 _ | E_hash_key _
      | E_address_of_contract _
      | E_create_contract_address (_, _, _, _)
      | E_hd _ | E_tl _ | E_size _ | E_isnat _ | E_int_of_nat _
      | E_lambda (_, _, _, _)
      | E_exec (_, _)
      | E_nil _ | E_empty_set _
      | E_empty_map (_, _)
      | E_empty_big_map (_, _)
      | E_apply (_, _)
      | E_append (_, _)
      | E_special_empty_list _
      | E_special_empty_map (_, _) ) as e ->
        Single
          (D.expr
             (fun v ->
               match abs_var v with
               | Option.None -> None
               | Some (Single x) -> Some x
               | _ -> Some D.bottom)
             e)

  let rec to_string = function
    | Single x -> D.to_string x
    | None -> "None"
    | Top -> "top"
    | Bottom -> "bottom"
    | Pair (x_1, x_2) -> [%string "(%{to_string x_1}, %{to_string x_2})"]
    | Left x -> [%string "Left %{to_string x}"]
    | Right x -> [%string "Right %{to_string x}"]
    | Or (x_1, x_2) -> [%string "Or (%{to_string x_1}, %{to_string x_2}"]
    | Some x -> [%string "Some %{to_string x}"]
    | Option x -> [%string "Option %{to_string x}"]
end
