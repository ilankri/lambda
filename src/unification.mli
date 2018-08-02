exception Occurs_check_failure of Var.t * Term.t

exception Symbol_name_conflict of string * string

exception Symbol_arity_conflict of string * int * int

val unify : (Term.t * Term.t) list -> (Substitution.t, exn) result
