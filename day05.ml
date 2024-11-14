let eval_single_input_multiple_outputs m input_value =
  let input_done = ref false in
  let input () =
    assert (not !input_done);
    input_done := true;
    input_value
  in
  let outputs = ref [] in
  let output x = outputs := x :: !outputs in
  Intcode.eval_io_ m ~input ~output;
  List.rev !outputs
;;

let f1 s =
  let m = Intcode.parse s in
  eval_single_input_multiple_outputs m 1 |> List.filter ~f:(fun n -> n <> 0) |> Algo.sole
;;

let f2 s =
  let m = Intcode.parse s in
  eval_single_input_multiple_outputs m 5 |> Algo.sole
;;

let run () = Run.run ~f1 ~f2 Day05_input.data
