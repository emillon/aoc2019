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

let next k done_ ~i ~o =
  let vo =
    match (!k : Intcode.k) with
    | Interpret s -> Some (Intcode.interpret_step s)
    | Input f -> Option.map ~f (Queue.dequeue i)
    | Output (v, k) ->
      Queue.enqueue o v;
      Some k
    | Halted ->
      done_ := true;
      None
  in
  match vo with
  | None -> ()
  | Some v -> k := v
;;

let run_feedback code pa pb pc pd pe =
  let k0 = Intcode.Interpret (Intcode.create_state code) in
  let sea = Queue.of_list [ pa; 0 ] in
  let sab = Queue.singleton pb in
  let sbc = Queue.singleton pc in
  let scd = Queue.singleton pd in
  let sde = Queue.singleton pe in
  let ka = ref k0 in
  let kb = ref k0 in
  let kc = ref k0 in
  let kd = ref k0 in
  let ke = ref k0 in
  let done_a = ref false in
  let done_b = ref false in
  let done_c = ref false in
  let done_d = ref false in
  let done_e = ref false in
  while not (!done_a && !done_b && !done_c && !done_d && !done_e) do
    next ka done_a ~i:sea ~o:sab;
    next kb done_b ~i:sab ~o:sbc;
    next kc done_c ~i:sbc ~o:scd;
    next kd done_d ~i:scd ~o:sde;
    next ke done_e ~i:sde ~o:sea
  done;
  Queue.dequeue_exn sea
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
