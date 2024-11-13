let f1 s =
  let m = Intcode.parse s in
  let exception Done of int in
  let output n = if n <> 0 then raise (Done n) in
  try
    Intcode.eval_io m ~input:1 ~output;
    assert false
  with
  | Done n -> n
;;

let f2 s =
  let m = Intcode.parse s in
  let exception Done of int in
  let output n = raise (Done n) in
  try
    Intcode.eval_io m ~input:5 ~output;
    assert false
  with
  | Done n -> n
;;

let run () = Run.run ~f1 ~f2 Day05_input.data
