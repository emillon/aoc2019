let run_once p input_value =
  let exception Done of int in
  let input_done = ref false in
  let input () =
    assert (not !input_done);
    input_done := true;
    input_value
  in
  let output n = raise (Done n) in
  try
    Intcode.eval_io_ p ~input ~output;
    assert false
  with
  | Done n -> n
;;

let f1 s =
  let p = Intcode.parse s in
  run_once p 1
;;

let f2 s =
  let p = Intcode.parse s in
  run_once p 2
;;

let run () = Run.run ~f1 ~f2 Day09_input.data
