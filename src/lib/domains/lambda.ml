open! Core

type lambda = Lambda_data of int | Lambda_expr of int [@@deriving ord, sexp]

module Lambda = struct
  module T = struct
    type t = lambda [@@deriving ord, sexp]
  end

  include T
  module Set = Set.Make (T)

  let to_string = function
    | Lambda_data id -> [%string "Data %{id#Int}"]
    | Lambda_expr id -> [%string "Expr %{id#Int}"]
end

module P_set = Lattices.Powerset.Make (Lambda)
include Lattices.With_top.Make (P_set)

let is_bottom = leq bottom
let top = `Top
let type_abs _ = bottom
let param_storage_abs _ _ = bottom

let meet x y =
  match (x, y) with
  | `Top, x | x, `Top -> x
  | `Some x, `Some y -> `Some (Set.inter x y)

let widen = join

let leq x y =
  match (x, y) with
  | _, `Top -> true
  | `Top, `Some _ -> false
  | `Some x, `Some y -> P_set.leq x y

let expr _ e =
  let open Tezla.Adt in
  match e.Node.value with
  | E_lambda _ -> `Some (P_set.Elt.Set.singleton (Lambda_expr e.id))
  | E_push { value = { value = T_lambda _, _; _ }, D_instruction _; _ } ->
      `Some (P_set.Elt.Set.singleton (Lambda_data e.id))
  | _ -> bottom
