type t = string

let make s = s

let to_string v = v

let compare = compare

module Set = struct
  include Set.Make (struct
      type var = t
      type t = var
      let compare = compare
    end)
end
