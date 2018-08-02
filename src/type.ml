type t =
  | Int
  | Var of Var.t
  | Bool
  | Arrow of t * t
  | Prod of t * t

type scheme = Scheme of Var.Set.t * t

let ( --> ) t t' = Arrow (t, t')

let ( *** ) t t' = Prod (t, t')

let rec term_of_type = function
  | Int -> Term.Sym ("int", [])
  | Var x -> Term.Var x
  | Bool -> Term.Sym ("bool", [])
  | Arrow (t, t') -> Term.Sym ("arrow", List.map term_of_type [t; t'])
  | Prod (t, t') -> Term.Sym ("prod", List.map term_of_type [t; t'])

let rec type_of_term = function
  | Term.Var x -> Var x
  | Term.Sym ("int", []) -> Int
  | Term.Sym ("bool", []) -> Bool
  | Term.Sym ("arrow", [t; t']) -> Arrow (type_of_term t, type_of_term t')
  | Term.Sym ("prod", [t; t']) -> Prod (type_of_term t, type_of_term t')
  | Term.Sym _ -> assert false

let unify t t' = Unification.unify [(term_of_type t, term_of_type t')]

let apply_subst subst t =
  t |> term_of_type |> Substitution.apply subst |> type_of_term

(* Beware of variable capture!  *)
let apply_subst_s subst (Scheme (vars, t)) =
  Scheme (
    vars,
    t |> term_of_type |> Substitution.apply subst |> type_of_term
  )

let fresh_var' =
  let c = ref 0 in
  fun () ->
    incr c;
    Var.make ("t" ^ string_of_int !c)

let fresh_var () = Var (fresh_var' ())

let instantiate (Scheme (vars, t)) : t =
  Var.Set.fold (fun var t ->
      t |> apply_subst (Substitution.make var (Term.Var (fresh_var' ())))
    ) vars t

let vars (t : t) = t |> term_of_type |> Term.vars

let free_vars (Scheme (vs, t)) = Var.Set.diff (vars t) vs

let to_string t = t |> term_of_type |> Term.to_string

let to_string_s (Scheme (xs, t)) =
  let vars =
    match Var.Set.elements xs with
    | [] -> ""
    | xs -> "forall " ^ String.concat " " (List.map Var.to_string xs) ^ ". "
  in
  vars ^ (t |> term_of_type |> Term.to_string)

let monotype t = Scheme (Var.Set.empty, t)
