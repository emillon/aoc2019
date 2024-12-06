let dot a b =
  (* TODO overflow is possible *)
  let r = List.fold2_exn a b ~init:0 ~f:(fun acc a b -> acc + (a * b)) in
  Int.abs (Int.rem r 10)
;;

let pattern n ~len =
  assert (len > 0);
  let q = Queue.create () in
  let exception Done in
  let emit x =
    Queue.enqueue q x;
    if Queue.length q = len then raise Done
  in
  try
    for _ = 1 to n do
      emit 0
    done;
    while true do
      for _ = 1 to n + 1 do
        emit 1
      done;
      for _ = 1 to n + 1 do
        emit 0
      done;
      for _ = 1 to n + 1 do
        emit (-1)
      done;
      for _ = 1 to n + 1 do
        emit 0
      done
    done
  with
  | Done -> Queue.to_list q
;;

let%expect_test "pattern" =
  let t n len =
    let r = pattern n ~len in
    print_s [%message (n : int) (len : int) (r : int list)]
  in
  t 0 1;
  [%expect {| ((n 0) (len 1) (r (1))) |}];
  t 0 4;
  [%expect {| ((n 0) (len 4) (r (1 0 -1 0))) |}];
  t 0 6;
  [%expect {| ((n 0) (len 6) (r (1 0 -1 0 1 0))) |}];
  t 0 8;
  [%expect {| ((n 0) (len 8) (r (1 0 -1 0 1 0 -1 0))) |}];
  t 1 8;
  [%expect {| ((n 1) (len 8) (r (0 1 1 0 0 -1 -1 0))) |}];
  t 2 8;
  [%expect {| ((n 2) (len 8) (r (0 0 1 1 1 0 0 0))) |}];
  t 3 8;
  [%expect {| ((n 3) (len 8) (r (0 0 0 1 1 1 1 0))) |}];
  t 4 8;
  [%expect {| ((n 4) (len 8) (r (0 0 0 0 1 1 1 1))) |}];
  t 5 8;
  [%expect {| ((n 5) (len 8) (r (0 0 0 0 0 1 1 1))) |}];
  t 6 8;
  [%expect {| ((n 6) (len 8) (r (0 0 0 0 0 0 1 1))) |}];
  t 7 8;
  [%expect {| ((n 7) (len 8) (r (0 0 0 0 0 0 0 1))) |}]
;;

let phase a =
  let len = List.length a in
  List.range 0 len |> List.map ~f:(fun i -> dot a (pattern i ~len))
;;

let%expect_test "phase" =
  let r = ref [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
  let next () =
    let l = phase !r in
    l |> [%sexp_of: int list] |> print_s;
    r := l
  in
  next ();
  [%expect {| (4 8 2 2 6 1 5 8) |}];
  next ();
  [%expect {| (3 4 0 4 0 4 3 8) |}];
  next ();
  [%expect {| (0 3 4 1 5 5 1 8) |}];
  next ();
  [%expect {| (0 1 0 2 9 4 9 8) |}]
;;

let phase100_8 l =
  let l' = Fn.apply_n_times ~n:100 phase l in
  List.take l' 8 |> List.map ~f:Int.to_string |> String.concat |> Int.of_string
;;

let parse s =
  s
  |> String.strip
  |> String.to_list
  |> List.map ~f:(fun c -> Int.of_string (String.of_char c))
;;

let%expect_test "phase100" =
  let t s = parse s |> phase100_8 |> printf "%d" in
  t "80871224585914546619083218645595";
  [%expect {| 24176176 |}];
  t "19617804207202209144916044189917";
  [%expect {| 73745418 |}];
  t "69317163492948606335995924319873";
  [%expect {| 52432133 |}]
;;

let f1 s = parse s |> phase100_8
let f2 _ = 0
let run () = Run.run ~f1 ~f2 Day16_input.data
