let parse s =
  let lines = String.split_lines s in
  List.foldi
    lines
    ~init:(Set.empty (module Vec))
    ~f:(fun j init ->
      String.foldi ~init ~f:(fun i acc c ->
        match c with
        | '.' -> acc
        | '#' -> Set.add acc (i, j)
        | _ -> raise_s [%message "parse" (c : char)]))
;;

let subvectors (x, y) =
  let d = Algo.gcd (abs x) (abs y) in
  let mx = x / d in
  let my = y / d in
  List.range 1 d |> List.map ~f:(fun i -> i * mx, i * my)
;;

let order s src dst =
  let v = Vec.sub dst src in
  List.count (subvectors v) ~f:(fun v -> Set.mem s (Vec.add src v))
;;

let is_blocked s src dst = order s src dst > 0

let count_visible_from s src =
  Set.count s ~f:(fun dst -> not (Vec.equal src dst || is_blocked s src dst))
;;

let best_score s =
  Set.to_list s
  |> List.map ~f:(fun p -> p, count_visible_from s p)
  |> List.max_elt ~compare:(Comparable.lift Int.compare ~f:snd)
  |> Option.value_exn
;;

let f1 s = parse s |> best_score |> snd
let coord_code (x, y) = (100 * x) + y

type relative_coords =
  { order : int
  ; arg : float
  }
[@@deriving compare]

let vec_arg (x, y) =
  let open Float in
  atan2 (neg (of_int x)) (of_int y)
;;

let%expect_test "vec_arg" =
  let t v = vec_arg v /. Float.pi |> [%sexp_of: float] |> print_s in
  t (1, 0);
  [%expect {| -0.5 |}];
  t (0, 1);
  [%expect {| -0 |}];
  t (-1, 0);
  [%expect {| 0.5 |}];
  t (0, -1);
  [%expect {| -1 |}];
  ()
;;

let relative_coords s src dst =
  let v = Vec.sub dst src in
  let arg = vec_arg v in
  let order = order s src dst in
  { arg; order }
;;

let shoot_order s src =
  Set.remove s src
  |> Set.to_list
  |> List.map ~f:(fun dst -> dst, relative_coords s src dst)
  |> List.sort ~compare:(Comparable.lift [%compare: relative_coords] ~f:snd)
  |> List.mapi ~f:(fun i (p, _) -> p, i + 1)
;;

let%expect_test "shoot_order" =
  let test s src (dimx, dimy) =
    let s = parse (String.concat_lines s) in
    let m = shoot_order s src |> Map.of_alist_exn (module Vec) in
    for j = 0 to dimy - 1 do
      for i = 0 to dimx - 1 do
        let p = i, j in
        let s =
          match Map.find m p with
          | _ when Vec.equal p src -> "X"
          | None -> "."
          | Some n when n < 10 -> Int.to_string n
          | Some _ -> "#"
        in
        printf "%s" s
      done;
      printf "\n"
    done
  in
  test
    [ ".#....#####...#.."
    ; "##...##.#####..##"
    ; "##...#...#.#####."
    ; "..#.....#...###.."
    ; "..#.#.....#....##"
    ]
    (8, 3)
    (17, 5);
  [%expect
    {|
    .#....###24...#..
    ##...##.13#67..9#
    ##...#...5.8####.
    ..#.....X...###..
    ..#.#.....#....##
    |}]
;;

let f2 s =
  let s = parse s in
  let src = best_score s |> fst in
  let l = shoot_order s src in
  List.find_map_exn l ~f:(fun (p, o) -> if o = 200 then Some (coord_code p) else None)
;;

let run () = Run.run ~f1 ~f2 Day10_input.data
