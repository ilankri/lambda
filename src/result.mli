val bind : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result

val ( >>= ) : ('a, 'c) result -> ('a -> ('b, 'c) result) -> ('b, 'c) result

val map : ('a -> 'b) -> ('a, 'c) result -> ('b, 'c) result

val ( >|= ) : ('a, 'c) result -> ('a -> 'b) -> ('b, 'c) result

val catch : ('a -> 'b) -> 'a -> ('b, exn) result

val get : ('a, exn) result -> 'a

val get_or_else : ('b -> 'a) -> ('a, 'b) result -> 'a

val of_option_or_else : (unit -> 'b) -> 'a option -> ('a, 'b) result
