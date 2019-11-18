
module type VERTEX =
sig
type t
type label
val equal : t -> t -> bool
val create : label -> t
val label : t -> labell
end;;


