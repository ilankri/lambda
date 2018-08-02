type t =
  | Int
  | Var of Var.t
  | Bool
  | Arrow of t * t
  | Prod of t * t

type scheme = Scheme of Var.Set.t * t

val ( --> ) : t -> t -> t

val ( *** ) : t -> t -> t

val apply_subst : Substitution.t -> t -> t

val apply_subst_s : Substitution.t -> scheme -> scheme

val instantiate : scheme -> t

val fresh_var : unit -> t

val fresh_var' : unit -> Var.t

val unify : t -> t -> (Substitution.t, exn) result

val vars : t -> Var.Set.t

val free_vars : scheme -> Var.Set.t

val to_string : t -> string

val to_string_s : scheme -> string

val monotype : t -> scheme
