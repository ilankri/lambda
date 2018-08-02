let x = Term.Var (Var.make "x")

let y = Term.Var (Var.make "y")

let g x = Term.Sym ("g", [x])

let string_of_equation (t, t') =
  Printf.sprintf "%s = %s" (Term.to_string t) (Term.to_string t')

let string_of_equations equations =
  equations
  |> List.map string_of_equation
  |> String.concat "\n"

let test eqs =
  let open Result in
  Printf.printf
    "Unifying \n%s\n...\n" (string_of_equations eqs);
  print_endline (
    Result.get_or_else
      Printexc.to_string (Unification.unify eqs >|= Substitution.to_string)
  )

let test1 () =
  let b = Term.Sym ("b", []) in
  let c = Term.Sym ("c", []) in
  let f x y z = Term.Sym ("f", [x; y; z]) in
  let h x = Term.Sym ("h", [x]) in
  test [(f x (h b) c, f (g y) y c)]

let test2 () =
  let p t t' = Term.Sym ("prod", [t; t']) in
  let a = Term.Sym ("a", []) in
  test [(p x x, p a a); (x, y)]

let test3 () = test [(x, g x)]

let main =
  test1 ();
  print_newline ();
  test2 ();
  print_newline ();
  test3 ()
