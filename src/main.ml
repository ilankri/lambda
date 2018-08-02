let main () =
  let read_typecheck_print () =
    try
      let prog =
        read_line ()
        |> Lexing.from_string
        |> Parser.prog Lexer.token
      in
      print_endline (
        prog
        |> W.w TypingEnvironment.empty
        |> Result.map (fun (t, _) -> Type.to_string t)
        |> Result.get_or_else Printexc.to_string
        (* get_or_else *)
        (*   (W.w TypingEnvironment.empty prog >|= fun (t, _) -> Type.to_string t) *)
        (*   Printexc.to_string *)
      )
    with
    | e -> e |> Printexc.to_string |> print_endline
  in
  let rec loop () =
    print_string "lambda> ";
    read_typecheck_print ();
    loop ()
  in
  loop ()

let () = main ()
