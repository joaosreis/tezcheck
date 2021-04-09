type bound = Int of Bigint.t | N_inf | P_inf

type t = Empty | Interval of bound * bound

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

let bottom = Empty

let top = Interval (N_inf, P_inf)

let type_abs =
  let open Tezla.Adt.Typ in
  function
  | T_int -> Interval (N_inf, P_inf)
  | T_nat -> Interval (Int Bigint.zero, P_inf)
  | T_mutez ->
      Interval
        (Int Bigint.zero, Int (Bigint.of_int (Int64.to_int Int64.max_int)))
  | T_key | T_unit | T_signature | T_operation | T_chain_id | T_string | T_bytes
  | T_bool | T_key_hash | T_timestamp | T_address | T_option _ | T_list _
  | T_set _ | T_contract _
  | T_pair (_, _)
  | T_or (_, _)
  | T_lambda (_, _)
  | T_map (_, _)
  | T_big_map (_, _) ->
      bottom

let param_storage_abs _ _ = Empty

let join x y =
  match (x, y) with
  | Empty, x | x, Empty -> x
  | Interval (a, b), Interval (c, d) -> Interval (bound_min a c, bound_max b d)

let meet x y =
  match (x, y) with
  | Empty, _ | _, Empty -> Empty
  | Interval (a, b), Interval (c, d) ->
      if bound_lt b c || bound_lt d a then Empty
      else Interval (bound_max a c, bound_min b d)

let widen x y =
  match (x, y) with
  | Empty, x | x, Empty -> x
  | Interval (a, b), Interval (c, d) ->
      let a' = if bound_lt c a then N_inf else a in
      let b' = if bound_gt d b then P_inf else b in
      Interval (a', b')

let subset x y =
  match (x, y) with
  | Empty, _ -> true
  | Interval _, Empty -> false
  | Interval (a, b), Interval (c, d) ->
      (bound_gt a c || bound_eq a c) && (bound_lt b d || bound_eq b d)

let%test "subset [0,5] [0,6] = true" =
  subset
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))

let%test "subset [0,5] [0,5] = true" =
  subset
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 5)))

let%test "subset [1,5] [0,6] = true" =
  subset
    (Interval (Int Bigint.one, Int Bigint.(of_int 5)))
    (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))

let%test "subset [0,6] [1,7] = false" =
  not
    (subset
       (Interval (Int Bigint.zero, Int Bigint.(of_int 6)))
       (Interval (Int Bigint.one, Int Bigint.(of_int 7))))

let is_bottom = function Empty -> true | Interval _ -> false

let abs = function
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

let neg = function
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

let add x y =
  match (x, y) with
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

let%test "[-3;6] + [-8;-2] = [-11;8]" =
  add
    (Interval (Int Bigint.(of_int (-3)), Int Bigint.(of_int 6)))
    (Interval (Int Bigint.(of_int (-8)), Int Bigint.(of_int (-2))))
  = Interval (Int Bigint.(of_int (-11)), Int Bigint.(of_int 4))

let mul x y =
  match (x, y) with
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

let%test "[-3;6] * [-8;-2] = [-48;24]" =
  mul
    (Interval (Int Bigint.(of_int (-3)), Int Bigint.(of_int 6)))
    (Interval (Int Bigint.(of_int (-8)), Int Bigint.(of_int (-2))))
  = Interval (Int Bigint.(of_int (-48)), Int Bigint.(of_int 24))

let compare x y =
  match (x, y) with
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

let expr abs_var =
  let open Tezla.Adt in
  let var_not_found l v =
    Core_kernel.Debug.amf l "var not found: %s" v.Var.var_name;
    exit 1
  in
  let vars_not_found l v_1 v_2 =
    Core_kernel.Debug.amf l "var not found: %s, %s" v_1.Var.var_name
      v_2.Var.var_name;
    exit 1
  in
  function
  | E_push (D_int x, (T_int | T_nat | T_mutez)) -> Interval (Int x, Int x)
  | (E_dup v | E_var v | E_abs v | E_neg v) as o -> (
      let a =
        match abs_var v with Some a -> a | None -> var_not_found [%here] v
      in
      match o with
      | E_dup _ -> a
      | E_abs _ -> abs a
      | E_neg _ -> neg a
      | _ -> assert false)
  | ( E_add (v_1, v_2)
    | E_sub (v_1, v_2)
    | E_mul (v_1, v_2)
    | E_compare (v_1, v_2) ) as o -> (
      let a_1, a_2 =
        match (abs_var v_1, abs_var v_2) with
        | Some a_1, Some a_2 -> (a_1, a_2)
        | _ -> vars_not_found [%here] v_1 v_2
      in
      match o with
      | E_add _ -> add a_1 a_2
      | E_sub _ -> add a_1 (neg a_2)
      | E_mul _ -> mul a_1 a_2
      | E_compare _ -> compare a_1 a_2
      | _ -> assert false)
  | _ -> bottom

let bound_to_string = function
  | N_inf -> "-inf"
  | P_inf -> "+inf"
  | Int x -> Bigint.to_string x

let to_string = function
  | Empty -> "bottom"
  | Interval (x, y) -> [%string "[%{bound_to_string x}; %{bound_to_string y}]"]
