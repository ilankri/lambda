type type_var = Type_var of string

type rec_type =
  | Int
  | Bool
  | Var of type_var
  | Prod of rec_type * rec_type
  | Arrow of rec_type * rec_type
  | Rec of type_var * rec_type

let rec contractive = function
  | Int | Bool -> true
  | Prod (t, t') | Arrow (t, t') -> contractive t && contractive t'
  | Rec (x, t) -> failwith "TODO"
