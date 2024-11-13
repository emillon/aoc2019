let parse s =
  String.strip s
  |> String.split ~on:','
  |> List.mapi ~f:(fun i s -> i, Int.of_string s)
  |> Map.of_alist_exn (module Int)
;;

let ( .%{} ) m a = Map.find_exn m a
let ( .%{}<- ) m key data = Map.set m ~key ~data

type instr =
  | Add of
      { asrc1 : int
      ; asrc2 : int
      ; adst : int
      }
  | Mul of
      { asrc1 : int
      ; asrc2 : int
      ; adst : int
      }
  | Hlt

let decode m ip =
  match m.%{ip} with
  | 1 ->
    let asrc1 = m.%{ip + 1} in
    let asrc2 = m.%{ip + 2} in
    let adst = m.%{ip + 3} in
    Add { asrc1; asrc2; adst }, 4
  | 2 ->
    let asrc1 = m.%{ip + 1} in
    let asrc2 = m.%{ip + 2} in
    let adst = m.%{ip + 3} in
    Mul { asrc1; asrc2; adst }, 4
  | 99 -> Hlt, 1
  | opcode -> raise_s [%message "decode" (opcode : int)]
;;

let eval m =
  let rec go m ~ip =
    match decode m ip with
    | Add { asrc1; asrc2; adst }, len ->
      let m' = m.%{adst} <- m.%{asrc1} + m.%{asrc2} in
      go m' ~ip:(ip + len)
    | Mul { asrc1; asrc2; adst }, len ->
      let m' = m.%{adst} <- m.%{asrc1} * m.%{asrc2} in
      go m' ~ip:(ip + len)
    | Hlt, _ -> Map.find_exn m 0
  in
  go m ~ip:0
;;

let eval_inputs (noun, verb) m =
  m |> Map.set ~key:1 ~data:noun |> Map.set ~key:2 ~data:verb |> eval
;;

let f1 s = parse s |> eval_inputs (12, 2)

let f2 s =
  let m = parse s in
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
