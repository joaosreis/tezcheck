type 'a t = Bottom | Top | Element of 'a

module Make (X : Sig.ELEMENT) = struct
  type property = X.t t

  let bottom = Bottom


  let equal x y =
    match (x, y) with
    | Bottom, Bottom | Top, Top -> true
    | Element x, Element y -> X.compare x y = 0
    | Top, (Bottom | Element _)
    | Bottom, (Top | Element _)
    | Element _, (Bottom | Top) ->
        false

  let lub x y =
    match (x, y) with
    | Bottom, _ | _, Top -> y
    | _ when equal x y -> y
    | _, Bottom | Top, _ -> x
    | Element _, Element _ -> Top

  let to_string = function
    | Bottom -> "bot"
    | Top -> "top"
    | Element x -> X.to_string x
end
