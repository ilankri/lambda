let bind x f =
  match x with
  | Some x -> f x
  | None -> None

let ( >>= ) = bind

let map f = function
  | Some x -> Some (f x)
  | None -> None

let ( >|= ) x f = map f x

let or_else x f =
  match x with
  | Some _ -> x
  | None -> f ()

let ( >>!= ) = or_else

let map_or_else f else' = function
  | Some x -> f x
  | None -> else' ()

let only_if c f = if c then Some (f ()) else None
