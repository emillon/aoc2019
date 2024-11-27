module Tile = struct
  type t =
    | Empty
    | Wall
    | Block
    | Paddle
    | Ball
  [@@deriving equal, sexp]

  let of_int = function
    | 0 -> Empty
    | 1 -> Wall
    | 2 -> Block
    | 3 -> Paddle
    | 4 -> Ball
    | n -> raise_s [%message "Tile.of_int" (n : int)]
  ;;
end

module Display = struct
  type t =
    { tiles : Tile.t Map.M(Vec).t
    ; score : int
    }
  [@@deriving sexp]

  let empty = { tiles = Map.empty (module Vec); score = 0 }
  let draw t p tile = { t with tiles = Map.set t.tiles ~key:p ~data:tile }
  let count_blocks t = Map.count t.tiles ~f:([%equal: Tile.t] Block)

  let get_position t tile =
    Map.to_alist t.tiles
    |> List.find_map_exn ~f:(fun ((x, _), t) ->
      Option.some_if ([%equal: Tile.t] tile t) x)
  ;;

  let ball_position t = get_position t Ball
  let paddle_position t = get_position t Paddle
  let set_score t score = { t with score }
  let score t = t.score
end

let interp c signal =
  let input () = assert false in
  let output n = Intcode.Signal.write signal n in
  Intcode.eval_io_ c ~input ~output
;;

type message =
  | Set_tile of Vec.t * Tile.t
  | Set_score of int
[@@deriving sexp]

let read_message s =
  let x = Intcode.Signal.read s in
  let y = Intcode.Signal.read s in
  let p = x, y in
  let id = Intcode.Signal.read s in
  if Vec.equal p (-1, 0) then Set_score id else Set_tile (p, Tile.of_int id)
;;

let run_display d s =
  while true do
    match read_message s with
    | Set_tile (p, t) -> d := Display.draw !d p t
    | Set_score s -> d := Display.set_score !d s
  done
;;

let f1 s =
  let c = Intcode.parse s in
  let signal = Intcode.Signal.create ~name:"display" in
  let d = ref Display.empty in
  Intcode.Scheduler.run_yield (fun () ->
    Intcode.Conc.fork (fun () -> interp c signal);
    Intcode.Conc.fork (fun () -> run_display d signal);
    Intcode.Conc.stop ());
  Display.count_blocks !d
;;

let joystick_from_display d =
  Int.compare (Display.ball_position d) (Display.paddle_position d)
;;

let f2 s =
  let c = Intcode.parse s |> Map.set ~key:0 ~data:2 in
  let signal = Intcode.Signal.create ~name:"display" in
  let done_sig = Intcode.Signal.create ~name:"done" in
  let d = ref Display.empty in
  Intcode.Scheduler.run_yield (fun () ->
    Intcode.Conc.fork (fun () -> run_display d signal);
    Intcode.Conc.fork (fun () ->
      Intcode.eval_io_
        c
        ~input:(fun () ->
          Intcode.Conc.yield ();
          joystick_from_display !d)
        ~output:(fun n -> Intcode.Signal.write signal n);
      Intcode.Signal.write done_sig 1);
    let (_ : int) = Intcode.Signal.read done_sig in
    Intcode.Conc.stop ());
  Display.score !d
;;

let run () = Run.run ~f1 ~f2 Day13_input.data
