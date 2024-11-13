type t = int Map.M(Int).t

val parse : string -> t
val eval : t -> int
val eval_io : t -> input:int -> output:(int -> unit) -> unit
