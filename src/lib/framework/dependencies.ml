module Cfg = Tezla_cfg.Flow_graph.Cfg

module Forward = struct
  let outdep = Cfg.outflow

  let indep = Cfg.inflow

  let is_extremal = Cfg.is_extremal
end

module Backward = struct
  let outdep = Cfg.inflow

  let indep = Cfg.outflow

  let is_extremal = Cfg.is_extremalR
end

module InterForward (M : sig
  val is_after_call : int -> bool
end) =
struct
  let outdep = Cfg.outflow

  let indep g l = if M.is_after_call l then [] else Cfg.inflow g l

  let is_extremal = Cfg.is_extremal
end

module ContextSensitiveInterForward (M : sig
  val is_call_or_exit : int -> bool
end) =
struct
  let outdep g l = if M.is_call_or_exit l then [] else Cfg.outflow g l

  let indep _ _ = assert false

  let is_extremal = Cfg.is_extremal
end

module type S = sig
  val outdep : Cfg.t -> int -> int list

  val indep : Cfg.t -> int -> int list

  val is_extremal : Cfg.t -> int -> bool
end
