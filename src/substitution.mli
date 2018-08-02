type t

val make : Var.t -> Term.t -> t

val apply : t -> Term.t -> Term.t

val compose : t -> t -> t

val id : t

val to_string : t -> string
