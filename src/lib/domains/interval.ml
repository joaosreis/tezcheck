open! Core

type bound = Int of Bigint.t | N_inf | P_inf [@@deriving sexp_of]
type t = Empty | Interval of bound * bound [@@deriving sexp_of]

exception Invalid_interval of bound * bound

let bound_min x y =
  match (x, y) with
  | N_inf, _ | _, N_inf -> N_inf
  | P_inf, x | x, P_inf -> x
  | Int x, Int y -> Int (Bigint.min x y)

let bound_max x y =
  match (x, y) with
  | N_inf, x | x, N_inf -> x
  | P_inf, _ | _, P_inf -> P_inf
  | Int x, Int y -> Int (Bigint.max x y)

let bound_lt x y =
  match (x, y) with
  | P_inf, P_inf | N_inf, N_inf -> false
  | Int x, Int y -> Bigint.(x < y)
  | N_inf, _ | _, P_inf -> true
  | P_inf, _ | _, N_inf -> false

let bound_gt x y =
  match (x, y) with
  | P_inf, P_inf | N_inf, N_inf -> false
  | Int x, Int y -> Bigint.(x > y)
  | N_inf, _ | _, P_inf -> false
  | P_inf, _ | _, N_inf -> true

let bound_eq x y =
  match (x, y) with
  | P_inf, P_inf | N_inf, N_inf -> true
  | Int x, Int y -> Bigint.equal x y
  | _ -> false

let interval_invariant i =
  match i with
  | Empty -> i
  | Interval (b_1, b_2) ->
      if bound_lt b_1 b_2 || bound_eq b_1 b_2 then i
      else raise (Invalid_interval (b_1, b_2))

let bottom = Empty
let top = Interval (N_inf, P_inf)

let type_abs t =
  let i =
    match fst t with
    | Edo_adt.Typ.Int -> Interval (N_inf, P_inf)
    | Nat -> Interval (Int Bigint.zero, P_inf)
    | Mutez -> Interval (Int Bigint.zero, Int (Bigint.of_int64 Int64.max_value))
    | Key | Unit | Signature | Operation | Chain_id | String | Bytes | Bool
    | Key_hash | Timestamp | Address | Option _ | List _ | Set _ | Contract _
    | Pair (_, _)
    | Or (_, _)
    | Lambda (_, _)
    | Map (_, _)
    | Big_map (_, _)
    | Never | Bls12_381_g1 | Bls12_381_g2 | Bls12_381_fr | Chest | Chest_key
    | Ticket _ | Sapling_transaction _ | Sapling_state _ ->
        bottom
  in
  interval_invariant i

let param_storage_abs _ _ = interval_invariant Empty

let join x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, x | x, Empty -> x
    | Interval (a, b), Interval (c, d) -> Interval (bound_min a c, bound_max b d)
  in
  interval_invariant i

let meet x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, _ | _, Empty -> Empty
    | Interval (a, b), Interval (c, d) ->
        if bound_lt b c || bound_lt d a then Empty
        else Interval (bound_max a c, bound_min b d)
  in
  interval_invariant i

let widen x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, x | x, Empty -> x
    | Interval (a, b), Interval (c, d) ->
        let a' = if bound_lt c a then N_inf else a in
        let b' = if bound_gt d b then P_inf else b in
        Interval (a', b')
  in
  interval_invariant i

let leq x y =
  match (interval_invariant x, interval_invariant y) with
  | Empty, _ -> true
  | Interval _, Empty -> false
  | Interval (a, b), Interval (c, d) ->
      (bound_gt a c || bound_eq a c) && (bound_lt b d || bound_eq b d)

let%test "leq [0,5] [0,6] = true" =
  leq
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))

let%test "leq [0,5] [0,5] = true" =
  leq
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))

let%test "leq [1,5] [0,6] = true" =
  leq
    (Interval (Int Bigint.one, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))

let%test "leq [0,6] [1,7] = false" =
  not
    (leq
       (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))
       (Interval (Int Bigint.one, Int Bigint.(of_int 7))))

let is_bottom = function Empty -> true | Interval _ -> false

let abs i =
  let i =
    match interval_invariant i with
    | Empty -> Empty
    | Interval (N_inf, P_inf) -> Interval (Int Bigint.zero, P_inf)
    | Interval (N_inf, Int x) ->
        if Bigint.(x < zero) then Interval (Int (Bigint.neg x), P_inf)
        else Interval (Int Bigint.zero, P_inf)
    | Interval (Int x, P_inf) as a ->
        if Bigint.(x < zero) then Interval (Int Bigint.zero, P_inf) else a
    | Interval (Int x, Int y) as a ->
        if Bigint.(x >= zero) then a
        else if Bigint.(x < zero) && Bigint.(x > zero) then
          let max = Bigint.max (Bigint.abs x) y in
          Interval (Int Bigint.zero, Int max)
        else Interval (Int (Bigint.abs y), Int (Bigint.abs x))
    | Interval (_, N_inf) | Interval (P_inf, _) -> assert false
  in
  interval_invariant i

let neg i =
  let i =
    match interval_invariant i with
    | Empty -> Empty
    | Interval (a, b) ->
        let bound_neg = function
          | N_inf -> P_inf
          | P_inf -> N_inf
          | Int x -> Int (Bigint.neg x)
        in
        let a = bound_neg a in
        let b = bound_neg b in
        Interval (bound_min a b, bound_max a b)
  in
  interval_invariant i

let add x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, _ | _, Empty -> Empty
    | Interval (a, b), Interval (c, d) ->
        let bound_add x y =
          match (x, y) with
          | N_inf, N_inf -> N_inf
          | P_inf, P_inf -> P_inf
          | N_inf, Int _ | Int _, N_inf -> N_inf
          | P_inf, Int _ | Int _, P_inf -> P_inf
          | Int x, Int y -> Int Bigint.(x + y)
          | N_inf, P_inf | P_inf, N_inf -> N_inf
        in
        let l = bound_add a c in
        let h = match bound_add b d with N_inf -> P_inf | x -> x in
        Interval (l, h)
  in
  interval_invariant i

let%test "[-3;6] + [-8;-2] = [-11;8]" =
  let x =
    add
      (Interval (Int Bigint.(of_int (-3)), Int Bigint.(of_int 6)))
      (Interval (Int Bigint.(of_int (-8)), Int Bigint.(of_int (-2))))
  in
  let y = Interval (Int Bigint.(of_int (-11)), Int Bigint.(of_int 4)) in
  leq x y && leq y x

let mul x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, _ | _, Empty -> Empty
    | Interval (a, b), Interval (c, d) ->
        let bound_mul x y =
          match (x, y) with
          | N_inf, P_inf | P_inf, N_inf | N_inf, Int _ | Int _, N_inf -> N_inf
          | P_inf, Int _ | Int _, P_inf -> P_inf
          | N_inf, N_inf | P_inf, P_inf -> P_inf
          | Int x, Int y -> Int Bigint.(x * y)
        in
        let m_1 = bound_mul a c in
        let m_2 = bound_mul b d in
        let m_3 = bound_mul a d in
        let m_4 = bound_mul b c in
        Interval
          ( bound_min m_1 (bound_min m_2 (bound_min m_3 m_4)),
            bound_max m_1 (bound_max m_2 (bound_max m_3 m_4)) )
  in
  interval_invariant i

let%test "[-3;6] * [-8;-2] = [-48;24]" =
  let x =
    mul
      (Interval (Int Bigint.(of_int (-3)), Int Bigint.(of_int 6)))
      (Interval (Int Bigint.(of_int (-8)), Int Bigint.(of_int (-2))))
  in
  let y = Interval (Int Bigint.(of_int (-48)), Int Bigint.(of_int 24)) in
  leq x y && leq y x

let compare x y =
  let i =
    match (interval_invariant x, interval_invariant y) with
    | Empty, _ | _, Empty -> Empty
    | Interval (a, b), Interval (c, d) ->
        if bound_lt b c then
          Interval (Int Bigint.(of_int (-1)), Int Bigint.(of_int (-1)))
        else if bound_lt d a then
          Interval (Int Bigint.(of_int 1), Int Bigint.(of_int 1))
        else if bound_eq a b && bound_eq a c && bound_eq a d then
          Interval (Int Bigint.zero, Int Bigint.zero)
        else if bound_eq b c then
          Interval (Int Bigint.(of_int (-1)), Int Bigint.zero)
        else if bound_eq a d then Interval (Int Bigint.zero, Int Bigint.one)
        else Interval (Int Bigint.(of_int (-1)), Int Bigint.one)
  in
  interval_invariant i

let expr abs_var e =
  let open Tezla.Adt in
  let var_not_found l v =
    Debug.amf l "var not found: %s" v.Var.var_name;
    exit 1
  in
  let vars_not_found l v_1 v_2 =
    Debug.amf l "var not found: %s, %s" v_1.Var.var_name v_2.Var.var_name;
    exit 1
  in
  let i =
    match e.Node.value with
    | E_push
        {
          value =
            ( {
                value =
                  ( ( T_int | T_nat | T_mutez | T_timestamp | T_bls12_381_g1
                    | T_bls12_381_g2 | T_bls12_381_fr ),
                    _ );
                _;
              },
              D_int x );
          _;
        } ->
        Interval (Int x, Int x)
    | E_dup v | E_dup_n (_, v) | E_var v -> (
        match abs_var v with Some a -> a | None -> var_not_found [%here] v)
    | E_abs v ->
        let a =
          match abs_var v with Some a -> a | None -> var_not_found [%here] v
        in
        abs a
    | E_neg_nat v
    | E_neg_int v
    | E_neg_bls12_381_g1 v
    | E_neg_bls12_381_g2 v
    | E_neg_bls12_381_fr v ->
        let a =
          match abs_var v with Some a -> a | None -> var_not_found [%here] v
        in
        neg a
    | E_add_bls12_381_fr (v_1, v_2)
    | E_add_bls12_381_g1 (v_1, v_2)
    | E_add_bls12_381_g2 (v_1, v_2)
    | E_add_int (v_1, v_2)
    | E_add_mutez (v_1, v_2)
    | E_add_nat (v_1, v_2)
    | E_add_nat_int (v_1, v_2)
    | E_add_timestamp_int (v_1, v_2) ->
        let a_1, a_2 =
          match (abs_var v_1, abs_var v_2) with
          | Some a_1, Some a_2 -> (a_1, a_2)
          | _ -> vars_not_found [%here] v_1 v_2
        in
        add a_1 a_2
    | E_sub_int (v_1, v_2)
    | E_sub_mutez (v_1, v_2)
    | E_sub_nat (v_1, v_2)
    | E_sub_nat_int (v_1, v_2)
    | E_sub_timestamp (v_1, v_2)
    | E_sub_timestamp_int (v_1, v_2) ->
        let a_1, a_2 =
          match (abs_var v_1, abs_var v_2) with
          | Some a_1, Some a_2 -> (a_1, a_2)
          | _ -> vars_not_found [%here] v_1 v_2
        in
        add a_1 (neg a_2)
    | E_mul_bls12_381_fr_bls12_381_fr (v_1, v_2)
    | E_mul_bls12_381_g1_bls12_381_fr (v_1, v_2)
    | E_mul_bls12_381_g2_bls12_381_fr (v_1, v_2)
    | E_mul_int (v_1, v_2)
    | E_mul_mutez_nat (v_1, v_2)
    | E_mul_int_bls12_381_fr (v_1, v_2)
    | E_mul_nat (v_1, v_2)
    | E_mul_nat_bls12_381_fr (v_1, v_2)
    | E_mul_nat_int (v_1, v_2) ->
        let a_1, a_2 =
          match (abs_var v_1, abs_var v_2) with
          | Some a_1, Some a_2 -> (a_1, a_2)
          | _ -> vars_not_found [%here] v_1 v_2
        in
        mul a_1 a_2
    | E_compare (v_1, v_2) ->
        let a_1, a_2 =
          match (abs_var v_1, abs_var v_2) with
          | Some a_1, Some a_2 -> (a_1, a_2)
          | _ -> vars_not_found [%here] v_1 v_2
        in
        compare a_1 a_2
    | _ -> bottom
  in
  interval_invariant i

let bound_to_string = function
  | N_inf -> "-inf"
  | P_inf -> "+inf"
  | Int x -> Bigint.to_string x

let to_string i =
  match interval_invariant i with
  | Empty -> "bottom"
  | Interval (x, y) -> [%string "[%{bound_to_string x}; %{bound_to_string y}]"]
