type t = int * int [@@deriving sexp]

include Comparable.S with type t := t
include Hashable.Key with type t := t

val zero : t
val one : t
val add : t -> t -> t
val cmul : t -> t -> t
val l1_norm : t -> int
val neighbours4 : t -> t list
