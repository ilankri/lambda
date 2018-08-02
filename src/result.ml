let bind r f =
  match r with
  | Ok r -> f r
  | Error _ as e -> e

let ( >>= ) = bind

let map f = function
  | Ok r -> Ok (f r)
  | Error _ as e -> e

let ( >|= ) r f = map f r

let catch f x = try Ok (f x) with e -> Error e

let get = function
  | Ok r -> r
  | Error e -> raise e

let get_or_else f r =
  match r with
  | Ok r -> r
  | Error e -> f e

let of_option_or_else f x =
  match x with
  | Some x -> Ok x
  | None -> Error (f ())
