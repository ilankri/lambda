let main =
  let vy = Var.make "y" in
  let vz = Var.make "z" in
  let y = Term.Var vy in
  let z = Term.Var vz in
  let b = Term.Sym ("b", []) in
  let c = Term.Sym ("c", []) in
  let f x = Term.Sym ("f", [x]) in
  let h x = Term.Sym ("h", [x]) in
  let s =
    Substitution.compose
      (Substitution.make vy b)
      (Substitution.make vz (h c))
  in
  let s' =
    Substitution.compose
      (Substitution.make (Var.make "x") (f y))
      (Substitution.make vy z)
  in
  Printf.printf "s:\n%s\ns':\n%s\n"
    (Substitution.to_string s) (Substitution.to_string s');
  Printf.printf "compose s s':\n%s\n"
    (Substitution.compose s s' |> Substitution.to_string)
