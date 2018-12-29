type t =
  | Int
  | Var of Var.t
  | Bool
  | Arrow of t * t
  | Prod of t * t

let ( --> ) t t' = Arrow (t, t')

let ( *** ) t t' = Prod (t, t')

let rec to_term = function
  | Int -> Term.Sym ("int", [])
  | Var x -> Term.Var x
  | Bool -> Term.Sym ("bool", [])
  | Arrow (t, t') -> Term.Sym ("arrow", List.map to_term [t; t'])
  | Prod (t, t') -> Term.Sym ("prod", List.map to_term [t; t'])

let rec from_term = function
  | Term.Var x -> Var x
  | Term.Sym ("int", []) -> Int
  | Term.Sym ("bool", []) -> Bool
  | Term.Sym ("arrow", [t; t']) -> Arrow (from_term t, from_term t')
  | Term.Sym ("prod", [t; t']) -> Prod (from_term t, from_term t')
  | Term.Sym _ -> assert false

let unify t t' = Unification.unify [(to_term t, to_term t')]

let apply_subst subst t =
  t |> to_term |> Substitution.apply subst |> from_term

let fresh_var () = Var (Var.fresh ())

let vars (t : t) = t |> to_term |> Term.vars

let to_string t = t |> to_term |> Term.to_string

module Scheme = struct
  type type_ = t
  type t = Scheme of Var.Set.t * type_

  let make vars t = Scheme (vars, t)

  let instantiate (Scheme (vars, t)) : type_ =
    Var.Set.fold (fun var t ->
      apply_subst (Substitution.make var (Term.Var (Var.fresh ()))) t
    ) vars t

  (* Beware of variable capture!  *)
  let apply_subst subst (Scheme (vars, t)) =
    make
      vars
      (t |> to_term |> Substitution.apply subst |> from_term)

  let free_vars (Scheme (vs, t)) = Var.Set.diff (vars t) vs

  let to_string (Scheme (xs, t)) =
    let vars =
      match Var.Set.elements xs with
      | [] -> ""
      | xs -> "forall " ^ String.concat " " (List.map Var.to_string xs) ^ ". "
    in
    vars ^ (t |> to_term |> Term.to_string)

  let monotype t = make Var.Set.empty t
end
