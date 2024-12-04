module Dir = struct
  type t =
    | North
    | South
    | East
    | West
  [@@deriving sexp]

  let all = [ North; South; East; West ]

  let to_vec = function
    | North -> 0, 1
    | South -> 0, -1
    | East -> 1, 0
    | West -> -1, 0
  ;;

  let to_int = function
    | North -> 1
    | South -> 2
    | West -> 3
    | East -> 4
  ;;
end

type status =
  | Wall
  | Oxygen
  | Moved
[@@deriving sexp]

let status_of_int = function
  | 0 -> Wall
  | 1 -> Moved
  | 2 -> Oxygen
  | n -> raise_s [%message "status_of_int" (n : int)]
;;

let play_trace c t =
  let q = Queue.of_list (List.rev t) in
  let exception R of status in
  try
    Intcode.eval_io_
      c
      ~input:(fun () -> Queue.dequeue_exn q |> Dir.to_int)
      ~output:(fun n ->
        let st = status_of_int n in
        if Queue.is_empty q
        then raise (R st)
        else if n = 0
        then raise_s [%message (q : Dir.t Queue.t) (st : status)]);
    assert false
  with
  | R st -> st
;;

let robot_bfs c ~f =
  let q = Queue.create () in
  Queue.enqueue q (Vec.zero, []);
  let seen_set = Hash_set.create (module Vec) in
  let seen pos = Hash_set.mem seen_set pos in
  let mark_seen pos = Hash_set.add seen_set pos in
  while not (Queue.is_empty q) do
    let pos, trace = Queue.dequeue_exn q in
    if not (seen pos)
    then (
      mark_seen pos;
      List.filter_map Dir.all ~f:(fun dir ->
        let trace = dir :: trace in
        let pos = Vec.add pos (Dir.to_vec dir) in
        f (play_trace c trace) pos trace)
      |> Queue.enqueue_all q)
  done;
  seen_set
;;

let f1 s =
  let c = Intcode.parse s in
  let exception Found of int in
  try
    let _ =
      robot_bfs c ~f:(fun status pos trace ->
        match status with
        | Wall -> None
        | Oxygen -> raise (Found (List.length trace))
        | Moved -> Some (pos, trace))
    in
    assert false
  with
  | Found n -> n
;;

let explore_all c =
  let oxygen = ref None in
  let seen_set =
    robot_bfs c ~f:(fun status pos trace ->
      match status with
      | Wall -> None
      | Oxygen ->
        oxygen := Some pos;
        Some (pos, trace)
      | Moved -> Some (pos, trace))
  in
  Option.value_exn !oxygen, seen_set
;;

let f2 s =
  let c = Intcode.parse s in
  let oxygen, empty_spots = explore_all c in
  let q = Queue.create () in
  Queue.enqueue q (oxygen, 0);
  let seen_set = Hash_set.create (module Vec) in
  let seen pos = Hash_set.mem seen_set pos in
  let mark_seen pos = Hash_set.add seen_set pos in
  let max_tick = ref Int.min_value in
  while not (Queue.is_empty q) do
    let pos, ticks = Queue.dequeue_exn q in
    if not (seen pos)
    then (
      mark_seen pos;
      max_tick := Int.max !max_tick ticks;
      List.filter_map Dir.all ~f:(fun dir ->
        let pos = Vec.add pos (Dir.to_vec dir) in
        Option.some_if (Hash_set.mem empty_spots pos) (pos, ticks + 1))
      |> Queue.enqueue_all q)
  done;
  !max_tick
;;

let run () = Run.run ~f1 ~f2 Day15_input.data
