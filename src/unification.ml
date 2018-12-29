let vars_equation (t, t') =
  Var.Set.empty |> Var.Set.union (Term.vars t) |> Var.Set.union (Term.vars t')

let as_var = Term.(function
  | Var x -> x
  | Sym _ -> assert false
)

let resolved_form equations =
  let lefts, rights = List.split equations in
  List.for_all Term.is_var lefts &&
  ExtList.all_different lefts &&
  List.for_all (fun var ->
    List.for_all (fun vars ->
      not (Var.Set.mem var vars)
    ) (List.map Term.vars rights)
  ) (List.map as_var lefts)

let subst_of_equations equations = Substitution.(
  let vars, terms = List.split equations in
  let vars = List.map as_var vars in
  List.fold_left2 (fun s x t -> compose s (make x t)) id vars terms
)

let do_eliminate x t equations =
  let s = Substitution.make x t in
  List.map (fun (t, t') -> Substitution.(apply s t, apply s t')) equations

let occurs x t = Var.Set.mem x (Term.vars t)

let rec try_rule rule = function
  | [] -> None
  | eq :: eqs -> (
      match rule eq with
      | Some r -> Some (r, eq, eqs)
      | None ->
          let open Option in
          try_rule rule eqs >>= fun (r, eq', eqs) ->
          Some (r, eq', eq :: eqs)
    )

exception Occurs_check_failure of Var.t * Term.t

exception Symbol_name_conflict of string * string

exception Symbol_arity_conflict of string * int * int

let rec delete equations =
  try_rule (fun (t, t') ->
    Option.only_if (t = t') (fun () -> fun _ equations -> unify equations)
  ) equations

and swap equations =
  try_rule (function
    | Term.Sym _, Term.Var _ ->
        Some (fun (t, t') equations -> unify ((t', t) :: equations))
    | Term.Var _, Term.Var _ | Term.Var _, Term.Sym _ |
      Term.Sym _, Term.Sym _ ->
        None
  ) equations

and decompose equations =
  try_rule (function
    | Term.Sym (f, ts), Term.Sym (f', ts') ->
        if f <> f'
        then raise (Symbol_name_conflict (f, f'))
        else
          let n = List.length ts in
          let n' = List.length ts' in
          if n <> n'
          then raise (Symbol_arity_conflict (f, n, n'))
          else
            Some (fun _ equations -> unify (equations @ List.combine ts ts'))
    | Term.Var _, Term.Var _ | Term.Var _, Term.Sym _ |
      Term.Sym _, Term.Var _ ->
        None
  ) equations

and eliminate equations =
  try_rule (fun (t, t') ->
    match t with
    | Term.Var x ->
        let exists_in_other_equations x equations =
          equations
          |> List.filter (fun eq -> Var.Set.mem x (vars_equation eq))
          |> List.length
          |> ( < ) 1
        in
        if occurs x t'
        then raise (Occurs_check_failure (x, t'))
        else
          Option.only_if (exists_in_other_equations x equations) (fun () ->
            fun equation equations ->
              unify (do_eliminate x t' equations @ [equation])
          )
    | Term.Sym _ -> None
  ) equations

and next_step equations =
  let open Option in
  delete equations >>!= fun () ->
  swap equations >>!= fun () ->
  decompose equations >>!= fun () ->
  eliminate equations

and unify equations =
  match next_step equations with
  | None -> equations
  | Some (rule, equation, equations) -> rule equation equations

let unifier subst equations =
  List.for_all (fun (t, t') ->
    Substitution.apply subst t = Substitution.apply subst t'
  ) equations

let unify equations =
  Result.catch (fun equations ->
    let equations = unify equations in
    let subst =
      assert (resolved_form equations);
      subst_of_equations equations
    in
    assert (unifier subst equations);
    subst
  ) equations
