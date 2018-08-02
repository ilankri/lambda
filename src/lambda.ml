type id = Id of string

type cst =
  | Fst
  | Snd
  | Fix
  | If_then_else
  | Add
  | Mult
  | Sub
  | C_bool of bool
  | C_int of int

type expr =
  | Var of id
  | Cst of cst
  | Pair of expr * expr
  | App of expr * expr
  | Abstr of id * Type.t option * expr
  | Let of id * Type.t option * expr * expr

let if_then_else c e e' = App (Cst If_then_else, Pair (c, Pair (e, e')))

let operator c e = App (Cst c, e)

let binop c e e' = App (Cst c, Pair (e, e'))

let mult e e' = binop Mult e e'

let add e e' = binop Add e e'

let sub e e' = binop Sub e e'

let fst e = operator Fst e

let snd e = operator Snd e

let fix e = operator Fix e

let int i = Cst (C_int i)

let bool b = Cst (C_bool b)
