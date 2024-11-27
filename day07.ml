type input_state =
  | Init
  | After1
  | After2

let run_amplifier code signal phase =
  let state = ref Init in
  let input () =
    let r, new_state =
      match !state with
      | Init -> phase, After1
      | After1 -> signal, After2
      | After2 -> assert false
    in
    state := new_state;
    r
  in
  let output_value = ref None in
  let output v =
    assert (Option.is_none !output_value);
    output_value := Some v
  in
  Intcode.eval_io_ code ~input ~output;
  Option.value_exn !output_value
;;

let run_sequence code l = List.fold l ~init:0 ~f:(run_amplifier code)

let f1 s =
  let code = Intcode.parse s in
  Algo.permutations [ 0; 1; 2; 3; 4 ]
  |> List.map ~f:(run_sequence code)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let run_feedback code pa pb pc pd pe =
  let e_to_a = Intcode.Signal.create ~name:"e_to_a" in
  let a_to_b = Intcode.Signal.create ~name:"a_to_b" in
  let b_to_c = Intcode.Signal.create ~name:"b_to_c" in
  let c_to_d = Intcode.Signal.create ~name:"c_to_d" in
  let d_to_e = Intcode.Signal.create ~name:"d_to_e" in
  let done_a = Intcode.Signal.create ~name:"a_done" in
  let done_b = Intcode.Signal.create ~name:"b_done" in
  let done_c = Intcode.Signal.create ~name:"c_done" in
  let done_d = Intcode.Signal.create ~name:"d_done" in
  let done_e = Intcode.Signal.create ~name:"e_done" in
  let start in_signal out_signal done_signal p =
    Intcode.Conc.fork (fun () ->
      Intcode.Signal.write in_signal p;
      Intcode.eval_scheduler code ~in_signal ~out_signal;
      Intcode.Signal.write done_signal 1)
  in
  let wait done_signal =
    let (_ : int) = Intcode.Signal.read done_signal in
    ()
  in
  let r = ref None in
  Intcode.Scheduler.run_yield (fun () ->
    start e_to_a a_to_b done_a pa;
    start a_to_b b_to_c done_b pb;
    start b_to_c c_to_d done_c pc;
    start c_to_d d_to_e done_d pd;
    start d_to_e e_to_a done_e pe;
    Intcode.Signal.write e_to_a 0;
    wait done_a;
    wait done_b;
    wait done_c;
    wait done_d;
    wait done_e;
    r := Some (Intcode.Signal.read e_to_a));
  Option.value_exn !r
;;

let f2 s =
  let code = Intcode.parse s in
  Algo.permutations [ 5; 6; 7; 8; 9 ]
  |> List.map ~f:(function
    | [ a; b; c; d; e ] -> run_feedback code a b c d e
    | _ -> assert false)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn
;;

let run () = Run.run ~f1 ~f2 Day07_input.data
