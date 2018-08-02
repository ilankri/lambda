let typeof cst =
  match cst with
  | Lambda.Mult | Lambda.Add | Lambda.Sub ->
      let open Type in
      (Type.Int *** Type.Int) --> Type.Int
  | Lambda.C_bool _ -> Type.Bool
  | Lambda.C_int _ -> Type.Int
  | Lambda.Fst | Lambda.Snd | Lambda.Fix | Lambda.If_then_else ->
      failwith "No polymorphism."

let rec type_check expr env =
  let open Option in
  let open Type in
  match expr with
  | Lambda.Var id -> TypingEnvironment.lookup env id

  | Lambda.Cst cst -> Some (typeof cst)

  | Lambda.Pair (e1, e2) ->
      type_check e1 env >>= fun ty1 ->
      type_check e2 env >|= fun ty2 ->
      ty1 *** ty2

  | Lambda.App (func, arg) ->
      type_check func env >>= fun ty -> (
        match ty with
        | Type.Arrow (in_ty, out_ty) ->
            type_check arg env >>= fun ty ->
            Option.only_if (in_ty = ty) (fun () -> out_ty)
        | Type.Int | Type.Bool | Type.Prod _ | Type.Var _ -> None
      )

  | Lambda.Abstr (id, ty, body) ->
      ty >>= fun ty -> (* We need a type annotation for [id].  *)
      env |> TypingEnvironment.bind id ty |> type_check body >|= fun ty' ->
      ty --> ty'

  | Lambda.Let (id, ty, e', e) ->
      ty >>= fun ty -> (* We need a type annotation for [id].  *)
      type_check e' env >>= fun ty' ->
      if ty = ty' then
        env |> TypingEnvironment.bind id ty |> type_check e
      else
        None
