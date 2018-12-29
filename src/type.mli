type t =
  | Int
  | Var of Var.t
  | Bool
  | Arrow of t * t
  | Prod of t * t

val ( --> ) : t -> t -> t

val ( *** ) : t -> t -> t

val apply_subst : Substitution.t -> t -> t

val fresh_var : unit -> t

val unify : t -> t -> (Substitution.t, exn) result

val vars : t -> Var.Set.t

val to_string : t -> string

module Scheme : sig
  type type_ = t

  type t

  val make : Var.Set.t -> type_ -> t

  val apply_subst : Substitution.t -> t -> t

  val instantiate : t -> type_

  val free_vars : t -> Var.Set.t

  val to_string : t -> string

  val monotype : type_ -> t
end
