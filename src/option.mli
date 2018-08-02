val bind : 'a option -> ('a -> 'b option) -> 'b option

val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option

val map : ('a -> 'b) -> 'a option -> 'b option

val ( >|= ) : 'a option -> ('a -> 'b) -> 'b option

val or_else : 'a option -> (unit -> 'a option) -> 'a option

val ( >>!= ) : 'a option -> (unit -> 'a option) -> 'a option

val map_or_else : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b

val only_if : bool -> (unit -> 'a) -> 'a option
