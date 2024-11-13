let eval_inputs (noun, verb) m =
  m |> Map.set ~key:1 ~data:noun |> Map.set ~key:2 ~data:verb |> Intcode.eval
;;

let f1 s = Intcode.parse s |> eval_inputs (12, 2)

let f2 s =
  let m = Intcode.parse s in
  let inputs =
    let all = List.range 0 100 in
    let open List.Let_syntax in
    let%bind noun = all in
    let%map verb = all in
    noun, verb
  in
  List.find_map_exn inputs ~f:(fun (noun, verb) ->
    let r = eval_inputs (noun, verb) m in
    Option.some_if (r = 19690720) ((100 * noun) + verb))
;;

let run () = Run.run ~f1 ~f2 Day02_input.data
