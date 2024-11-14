let parse =
  let open Parsing_util in
  let open Angstrom in
  let obj = take_while1 Char.is_alphanum in
  parse_lines_using (both (obj <* char ')') obj)
;;

let sum_of_depths edges =
  let m =
    List.fold
      edges
      ~init:(Map.empty (module String))
      ~f:(fun m (a, b) -> Map.add_multi m ~key:a ~data:b)
  in
  let rec go start i =
    let next = Map.find_multi m start in
    i + List.fold next ~init:0 ~f:(fun sum next -> sum + go next (i + 1))
  in
  go "COM" 0
;;

let f1 s = parse s |> sum_of_depths

let%expect_test "f1" =
  [ "COM)B"; "B)C"; "C)D"; "D)E"; "E)F"; "B)G"; "G)H"; "D)I"; "E)J"; "J)K"; "K)L" ]
  |> String.concat_lines
  |> f1
  |> printf "%d";
  [%expect {| 42 |}]
;;

let rec different_tail_lens la lb =
  match la, lb with
  | ha :: ta, hb :: tb when String.equal ha hb -> different_tail_lens ta tb
  | _ -> List.length la + List.length lb
;;

let find_dist edges =
  let m = List.map edges ~f:(fun (a, b) -> b, a) |> Map.of_alist_exn (module String) in
  let rec path k =
    match Map.find m k with
    | None -> []
    | Some p -> k :: path p
  in
  let p1 = path "YOU" in
  let p2 = path "SAN" in
  different_tail_lens (List.rev p1) (List.rev p2) - 2
;;

let f2 s = parse s |> find_dist

let%expect_test "f2" =
  [ "COM)B"
  ; "B)C"
  ; "C)D"
  ; "D)E"
  ; "E)F"
  ; "B)G"
  ; "G)H"
  ; "D)I"
  ; "E)J"
  ; "J)K"
  ; "K)L"
  ; "K)YOU"
  ; "I)SAN"
  ]
  |> String.concat_lines
  |> f2
  |> printf "%d";
  [%expect {| 4 |}]
;;

let run () = Run.run ~f1 ~f2 Day06_input.data
