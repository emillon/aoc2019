let parse =
  let open Parsing_util in
  let open Angstrom in
  let dir = enum [ "U", (0, 1); "D", (0, -1); "L", (-1, 0); "R", (1, 0) ] in
  let leg = both dir number in
  let line = sep_by1 (char ',') leg <* end_of_line in
  let input = both line line in
  parse_using input
;;

let leg_elements (vdir, len) =
  List.range 1 len ~stop:`inclusive |> List.map ~f:(fun i -> Vec.cmul (i, 0) vdir)
;;

let fill l =
  let _, m, _ =
    List.fold
      l
      ~init:(Vec.zero, Map.empty (module Vec), 0)
      ~f:(fun (pos, m, dist) leg ->
        let leg_list = leg_elements leg in
        let leg_dist_list =
          List.mapi leg_list ~f:(fun i p -> Vec.add pos p, dist + i + 1)
        in
        let leg_dist_map = Map.of_alist_exn (module Vec) leg_dist_list in
        let new_pos, new_dist = List.last_exn leg_dist_list in
        let new_m =
          Map.merge_skewed m leg_dist_map ~combine:(fun ~key:_ da db -> Int.min da db)
        in
        new_pos, new_m, new_dist)
  in
  m
;;

let paint l = fill l |> Map.to_alist |> List.map ~f:fst |> Set.of_list (module Vec)

let closest (a, b) =
  let sa = paint a in
  let sb = paint b in
  let i = Set.inter sa sb in
  Set.fold ~init:Int.max_value i ~f:(fun m p ->
    if Vec.equal p Vec.zero then m else Int.min m (Vec.l1_norm p))
;;

let f1 s = parse s |> closest

let%expect_test "f1" =
  let test a b =
    let r = f1 (String.concat_lines [ a; b ]) in
    print_s [%message (r : int)]
  in
  test "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83";
  [%expect {| (r 159) |}];
  test
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
  [%expect {| (r 135) |}]
;;

let intersect a b =
  Map.merge a b ~f:(fun ~key:_ r ->
    match r with
    | `Left _ -> None
    | `Right _ -> None
    | `Both (da, db) -> Some (da + db))
;;

let closest2 (a, b) =
  let sa = fill a in
  let sb = fill b in
  let i = intersect sa sb in
  Map.fold ~init:Int.max_value i ~f:(fun ~key:_ ~data m -> Int.min m data)
;;

let f2 s = parse s |> closest2

let%expect_test "f2" =
  let test a b =
    let r = f2 (String.concat_lines [ a; b ]) in
    print_s [%message (r : int)]
  in
  test "R75,D30,R83,U83,L12,D49,R71,U7,L72" "U62,R66,U55,R34,D71,R55,D58,R83";
  [%expect {| (r 610) |}];
  test
    "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51"
    "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7";
  [%expect {| (r 410) |}]
;;

let run () = Run.run ~f1 ~f2 Day03_input.data
