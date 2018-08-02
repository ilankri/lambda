type 'a t = (Lambda.id * 'a) list

let empty = []

let bind id ty env = (id, ty) :: env

let lookup env id =
  try Some (List.assoc id env) with
  | Not_found -> None

let map f env = List.map (fun (id, t) -> (id, f t)) env

let fold f env acc = List.fold_left (fun acc (id, t) -> f id t acc) acc env
