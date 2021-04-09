(* open Batteries
open Tezla.Adt

type func = var * typ * typ

type token = var

type cnst =
  | C_a of var * var
  | C_b of var * var
  | C_c of var * var * (var * var) list

let generate_constraints n =
  let rec aux (constraints, functions) n =
    match n.stm with
    | S_assign (v, e) -> (
        match e with
        | E_lambda (t_1, t_2, _, _) ->
            (C_a (v, v) :: constraints, (v, t_1, t_2) :: functions)
        | E_dup v' -> (C_b (v', v) :: constraints, functions)
        | _ -> (constraints, functions))
    | _ -> (constraints, functions)
  in
  let (constraints, functions) = aux ([],[]) n in
  let rec aux constraints =
    match n.stm with
    | S_assign (v, e) -> (match e with E_apply ()) *)
