val w :
  Type.scheme TypingEnvironment.t ->
  Lambda.expr ->
  (Type.t * Substitution.t, exn) result
