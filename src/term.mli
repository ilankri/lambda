type t =
  | Var of Var.t
  | Sym of string * t list

val vars : t -> Var.Set.t

val to_string : t -> string

val is_var : t -> bool
