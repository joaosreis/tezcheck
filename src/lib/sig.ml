module type Element = sig
  type t

  val to_string : t -> string
end

module type Call_context = sig
  type t

  type vertex = Tezla_cfg.Cfg_node.t

  val to_string : t -> string

  val initial_context : t

  val make_call_context : t -> int -> 'a -> int -> t
end
