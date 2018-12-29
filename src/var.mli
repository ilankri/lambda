type t

val make : string -> t

val to_string : t -> string

val compare : t -> t -> int

val fresh : unit -> t

module Set : sig
  include Set.S with type elt = t
end
