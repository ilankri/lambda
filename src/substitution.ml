module VarMap = Map.Make (Var)

type t = Term.t VarMap.t

let make x t = VarMap.singleton x t

let rec apply s = Term.(function
  | Var x as var -> (
      try VarMap.find x s with
      | Not_found -> var
    )
  | Sym (f, ts) -> Sym (f, List.map (apply s) ts)
)

let compose s s' =
  VarMap.merge (fun _ t t' ->
    match t, t' with
    | None, None -> None
    | None, Some t | Some t, None | Some _, Some t -> Some t
  ) s (VarMap.map (apply s) s')

let id = VarMap.empty

let to_string s =
  VarMap.bindings s
  |> List.map (fun (x, t) -> Var.to_string x ^ " -> " ^ Term.to_string t)
  |> String.concat "\n"
