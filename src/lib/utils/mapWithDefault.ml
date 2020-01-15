open Batteries

module Make(D : sig type t val default : t end) = struct
  include Map

  let find x m = try find x m with Not_found -> D.default
end