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

val if_then_else : expr -> expr -> expr -> expr

val operator : cst -> expr -> expr

val binop : cst -> expr -> expr -> expr

val mult : expr -> expr -> expr

val add : expr -> expr -> expr

val sub : expr -> expr -> expr

val fst : expr -> expr

val snd : expr -> expr

val fix : expr -> expr

val int : int -> expr

val bool : bool -> expr
