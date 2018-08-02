let typeof (cst : Lambda.cst) : Type.scheme =
  let open Type in
  match cst with
  | Lambda.Fst | Lambda.Snd ->
      let alpha = Type.fresh_var' () in
      let beta = Type.fresh_var' () in
      let vars =
        Var.Set.union (Var.Set.singleton alpha) (Var.Set.singleton beta)
      in
      let alpha = Type.Var alpha in
      let beta = Type.Var beta in
      Type.Scheme (
        vars,
        (alpha *** beta) --> if cst = Lambda.Fst then alpha else beta
      )
  | Lambda.If_then_else ->
      let alpha = Type.fresh_var' () in
      let vars = Var.Set.singleton alpha in
      let alpha = Type.Var alpha in
      Type.Scheme (vars, (Type.Int *** (alpha *** alpha)) --> alpha)
  | Lambda.Fix ->
      let alpha = Type.fresh_var' () in
      let vars = Var.Set.singleton alpha in
      let alpha = Type.Var alpha in
      Type.Scheme (vars, (alpha --> alpha) --> alpha)
  | Lambda.Add | Lambda.Mult | Lambda.Sub ->
      Type.monotype ((Type.Int *** Type.Int) --> Type.Int)
  | Lambda.C_bool _ -> Type.monotype Type.Bool
  | Lambda.C_int _ -> Type.monotype Type.Int

let apply_subst_env s env = TypingEnvironment.map (Type.apply_subst_s s) env

let free_vars (env : Type.scheme TypingEnvironment.t) : Var.Set.t =
  TypingEnvironment.fold (fun _ s acc ->
      s |> Type.free_vars |> Var.Set.union acc
    ) env Var.Set.empty

let generalize (t : Type.t) (env : Type.scheme TypingEnvironment.t) :
  Type.scheme =
  Type.Scheme (Var.Set.diff (Type.vars t) (free_vars env), t)

exception Unbound_identifier of Lambda.id

let rec w env expr =
  let open Result in
  let open Type in
  match expr with
  | Lambda.Var x ->
      Result.of_option_or_else
        (fun () -> Unbound_identifier x)
        (TypingEnvironment.lookup env x)
      >|= fun s -> (Type.instantiate s, Substitution.id)
  | Lambda.Cst c -> Ok (typeof c |> Type.instantiate, Substitution.id)
  | Lambda.Pair (e, e') ->
      w env e >>= fun (b, rho_b) ->
      w (apply_subst_env rho_b env) e' >|= fun (c, rho_c) ->
      (Type.apply_subst rho_c b *** c, Substitution.compose rho_c rho_b)
  | Lambda.App (e, e') ->
      w env e >>= fun (b, rho_b) ->
      w (apply_subst_env rho_b env) e' >>= fun (c, rho_c) ->
      let alpha = Type.fresh_var () in
      let rho_c_b = Type.apply_subst rho_c b in
      Type.unify rho_c_b (c --> alpha) >|= fun mu ->
      (Type.apply_subst mu alpha,
       Substitution.compose mu (Substitution.compose rho_c rho_b))
  | Lambda.Abstr (id, None, e) ->
      let alpha = Type.fresh_var () in
      let env =
        TypingEnvironment.bind id (Type.Scheme (Var.Set.empty, alpha)) env
      in
      w env e >|= fun (b, rho) -> (Type.apply_subst rho alpha --> b, rho)
  | Lambda.Let (id, None, e, e') ->
      w env e >>= fun (b, rho_b) ->
      let env = apply_subst_env rho_b env in
      let env = TypingEnvironment.bind id (generalize b env) env in
      w env e' >|= fun (c, rho_c) ->
      (c, Substitution.compose rho_c rho_b)
  | Lambda.Abstr (_, Some _, _) |
    Lambda.Let (_, Some _, _, _) ->
      failwith "TODO"
