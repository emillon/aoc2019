module State = struct
  module T = struct
    type t =
      { walls : Set.M(Vec).t
      ; keys : char Map.M(Vec).t
      ; doors : Vec.t Map.M(Char).t
      ; doors_pos : Set.M(Vec).t
      ; pos : Vec.t
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let empty =
    { walls = Set.empty (module Vec)
    ; keys = Map.empty (module Vec)
    ; doors = Map.empty (module Char)
    ; doors_pos = Set.empty (module Vec)
    ; pos = -1, -1
    }
  ;;

  let add_wall t ~pos = { t with walls = Set.add t.walls pos }

  let add_door t ~pos ~c =
    { t with
      doors = Map.add_exn t.doors ~key:c ~data:pos
    ; doors_pos = Set.add t.doors_pos pos
    }
  ;;

  let add_key t ~pos ~c = { t with keys = Map.add_exn t.keys ~key:pos ~data:c }

  let set_entrance t ~pos =
    assert (Vec.equal t.pos empty.pos);
    { t with pos }
  ;;

  let is_valid_pos t pos = (not (Set.mem t.walls pos)) && not (Set.mem t.doors_pos pos)
  let is_valid t = is_valid_pos t t.pos
  let is_winning t = Map.is_empty t.keys

  let move_to t pos =
    match Map.find t.keys pos with
    | None -> { t with pos }
    | Some key ->
      let keys = Map.remove t.keys pos in
      let t = { t with keys; pos } in
      (match Map.find t.doors key with
       | Some door ->
         let walls = Set.remove t.walls door in
         let doors = Map.remove t.doors key in
         let doors_pos = Set.remove t.doors_pos door in
         { t with walls; doors; pos; doors_pos }
       | None -> t)
  ;;

  let all_dirs = [ 0, 1; 0, -1; 1, 0; -1, 0 ]

  type state = t [@@deriving sexp]

  module Precomputed : sig
    type t [@@deriving sexp]

    val create : state -> t
    val next : t -> src:Vec.t -> (Vec.t * (int * char list)) list
  end = struct
    type t = (Vec.t * (int * char list)) list Map.M(Vec).t [@@deriving sexp]

    let find_door t p =
      if Set.mem t.doors_pos p
      then
        Some
          (Map.to_alist t.doors
           |> List.find_map_exn ~f:(fun (key, door_pos) ->
             Option.some_if (Vec.equal door_pos p) key))
      else None
    ;;

    let collect_from t src =
      let q = Queue.create () in
      Queue.enqueue q (src, 0, []);
      let dist_map = ref (Map.empty (module Vec)) in
      let r = ref [] in
      while not (Queue.is_empty q) do
        let p, d, keys = Queue.dequeue_exn q in
        if Set.mem t.walls p
        then ()
        else (
          match Map.find !dist_map p with
          | Some _ -> ()
          | None ->
            dist_map := Map.add_exn !dist_map ~key:p ~data:d;
            if Map.mem t.keys p then r := (p, (d, keys)) :: !r;
            let keys' =
              match find_door t p with
              | Some c -> c :: keys
              | None -> keys
            in
            List.map all_dirs ~f:(fun dir -> Vec.add p dir, d + 1, keys')
            |> Queue.enqueue_all q)
      done;
      !r
    ;;

    let create t =
      let srcs = t.pos :: Map.keys t.keys in
      List.map srcs ~f:(fun key_pos ->
        let dists = collect_from t key_pos in
        key_pos, dists)
      |> Map.of_alist_exn (module Vec)
    ;;

    let next dist ~src = Map.find_exn dist src
  end

  let next t preco =
    let missing_keys =
      Map.fold
        t.keys
        ~init:(Set.empty (module Char))
        ~f:(fun ~key:_ ~data acc -> Set.add acc data)
    in
    Precomputed.next preco ~src:t.pos
    |> List.filter_map ~f:(fun (dst, (dist, keys)) ->
      Option.some_if
        (Map.mem t.keys dst
         && List.for_all keys ~f:(fun key -> not (Set.mem missing_keys key)))
        (move_to t dst, dist))
  ;;
end

let parse s =
  let lines = String.split_lines s in
  List.foldi lines ~init:State.empty ~f:(fun j acc line ->
    String.foldi line ~init:acc ~f:(fun i acc c ->
      let pos = i, j in
      match c with
      | '#' -> acc |> State.add_wall ~pos
      | '.' -> acc
      | 'A' .. 'Z' -> acc |> State.add_door ~pos ~c:(Char.lowercase c)
      | 'a' .. 'z' -> acc |> State.add_key ~pos ~c
      | '@' -> acc |> State.set_entrance ~pos
      | _ -> raise_s [%message "parse" (c : char)]))
;;

let f1 s =
  let t = parse s in
  let preco = State.Precomputed.create t in
  let q = Pairing_heap.create ~cmp:(Comparable.lift ~f:snd Int.compare) () in
  Pairing_heap.add q (t, 0);
  let set = ref (Set.empty (module State)) in
  let seen t = Set.mem !set t in
  let mark_seen t = set := Set.add !set t in
  let exception Found of int in
  try
    while not (Pairing_heap.is_empty q) do
      let t, steps = Pairing_heap.pop_exn q in
      if State.is_valid t && not (seen t)
      then (
        if State.is_winning t then raise (Found steps);
        mark_seen t;
        State.next t preco
        |> List.iter ~f:(fun (s, n) -> Pairing_heap.add q (s, steps + n)))
    done;
    assert false
  with
  | Found n -> n
;;

let%expect_test "f1" =
  let test l = String.concat_lines l |> f1 |> printf "%d" in
  test [ "#########"; "#b.A.@.a#"; "#########" ];
  [%expect {| 8 |}];
  test
    [ "########################"
    ; "#f.D.E.e.C.b.A.@.a.B.c.#"
    ; "######################.#"
    ; "#d.....................#"
    ; "########################"
    ];
  [%expect {| 86 |}];
  test
    [ "########################"
    ; "#...............b.C.D.f#"
    ; "#.######################"
    ; "#.....@.a.B.c.d.A.e.F.g#"
    ; "########################"
    ];
  [%expect {| 132 |}];
  test
    [ "#################"
    ; "#i.G..c...e..H.p#"
    ; "########.########"
    ; "#j.A..b...f..D.o#"
    ; "########@########"
    ; "#k.E..a...g..B.n#"
    ; "########.########"
    ; "#l.F..d...h..C.m#"
    ; "#################"
    ];
  [%expect {| 136 |}];
  test
    [ "########################"
    ; "#@..............ac.GI.b#"
    ; "###d#e#f################"
    ; "###A#B#C################"
    ; "###g#h#i################"
    ; "########################"
    ];
  [%expect {| 81 |}]
;;

let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day18_input.data
