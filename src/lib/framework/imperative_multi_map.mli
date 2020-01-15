type ('a, 'b) t

val create  : int -> ('a, 'b) t
val find    : ('a, 'b BatSet.t) t -> 'a -> 'b BatSet.t
val add     : ('a, 'b BatSet.t) t -> 'a -> 'b -> unit
