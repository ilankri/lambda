type 'a t

val empty : 'a t

val bind : Lambda.id -> 'a -> 'a t -> 'a t

val lookup : 'a t -> Lambda.id -> 'a option

val map : ('a -> 'b) -> 'a t -> 'b t

val fold : (Lambda.id -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
