type t = int Map.M(Int).t

val parse : string -> t
val eval : t -> int
val eval_io : t -> input:(unit -> int) -> output:(int -> unit) -> t
val eval_io_ : t -> input:(unit -> int) -> output:(int -> unit) -> unit

type state

val create_state : t -> state

module Signal : sig
  type t

  val create : name:string -> t
  val write : t -> int -> unit
  val read : t -> int
end

module Scheduler : sig
  val run_both : (unit -> unit) -> (unit -> unit) -> unit
  val run_yield : (unit -> unit) -> unit
end

module Conc : sig
  val fork : (unit -> unit) -> unit
  val stop : unit -> unit
  val yield : unit -> unit
end

val eval_scheduler : t -> in_signal:Signal.t -> out_signal:Signal.t -> unit
