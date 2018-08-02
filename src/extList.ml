let all_different l =
  let rec aux l =
    match l with
    | [] | [_] -> true
    | x :: x' :: xs -> x <> x' && aux (x' :: xs)
  in
  aux (List.sort compare l)
