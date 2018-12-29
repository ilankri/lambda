type t = string

let make s = s

let to_string v = v

let compare = compare

let fresh =
  let c = ref 0 in
  fun () ->
    incr c;
    make ("t" ^ string_of_int !c)

module Set = struct
  include Set.Make (struct
      type var = t
      type t = var
      let compare = compare
    end)
end
