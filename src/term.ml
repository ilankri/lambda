type t =
  | Var of Var.t
  | Sym of string * t list

let rec vars = function
  | Var x -> Var.Set.singleton x
  | Sym (_, ts) ->
      List.fold_left (fun acc t ->
        t |> vars |> Var.Set.union acc
      ) Var.Set.empty ts

let rec to_string = function
  | Var x -> Var.to_string x
  | Sym (f, ts) ->
      f ^
      match ts with
      | [] -> ""
      | _ -> "(" ^ String.concat ", " (List.map to_string ts) ^ ")"

let is_var = function
  | Var _ -> true
  | Sym _ -> false
