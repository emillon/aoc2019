type t = int Map.M(Int).t

val parse : string -> t
val eval : t -> int
val eval_io : t -> input:(unit -> int) -> output:(int -> unit) -> t
val eval_io_ : t -> input:(unit -> int) -> output:(int -> unit) -> unit

type state

val create_state : t -> state

type k =
  | Interpret of state
  | Input of (int -> k)
  | Output of int * k
  | Halted

val interpret_step : state -> k
