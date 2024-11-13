let parse s = String.split_lines s |> List.map ~f:Int.of_string
let fuel_for n = (n / 3) - 2
let f1 s = parse s |> List.fold ~init:0 ~f:(fun acc n -> fuel_for n + acc)

let total_fuel_for n =
  let rec go f =
    let r = fuel_for f in
    if r > 0 then f + go r else f
  in
  go (fuel_for n)
;;

let%expect_test "total_fuel_for" =
  let test n =
    let r = total_fuel_for n in
    print_s [%message (n : int) (r : int)]
  in
  test 14;
  [%expect {| ((n 14) (r 2)) |}];
  test 1969;
  [%expect {| ((n 1969) (r 966)) |}];
  test 100756;
  [%expect {| ((n 100756) (r 50346)) |}]
;;

let f2 s = parse s |> List.fold ~init:0 ~f:(fun acc n -> total_fuel_for n + acc)
let run () = Run.run ~f1 ~f2 Day01_input.data
