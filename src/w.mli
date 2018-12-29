val w :
  Type.Scheme.t TypingEnvironment.t ->
  Lambda.expr ->
  (Type.t * Substitution.t, exn) result
