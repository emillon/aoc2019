type pixel =
  | Black
  | White
  | Transparent
[@@deriving equal]

let pixel_of_char = function
  | '0' -> Black
  | '1' -> White
  | '2' -> Transparent
  | c -> raise_s [%message "pixel_of_char" (c : char)]
;;

let width = 25
let height = 6

let parse =
  let open Parsing_util in
  let open Angstrom in
  let layer =
    let+ s = take (width * height) in
    String.to_list s |> List.map ~f:pixel_of_char
  in
  let image = many1 layer <* end_of_line in
  parse_using image
;;

let count_pixels ~p = List.count ~f:([%equal: pixel] p)

let f1 s =
  let layers = parse s in
  let layer =
    List.min_elt layers ~compare:(Comparable.lift Int.compare ~f:(count_pixels ~p:Black))
    |> Option.value_exn
  in
  count_pixels layer ~p:White * count_pixels layer ~p:Transparent
;;

let display layer =
  List.iteri layer ~f:(fun i p ->
    if i % width = 0 then printf "|";
    let s =
      match p with
      | Black -> " "
      | White -> "▉"
      | Transparent -> assert false
    in
    printf "%s" s;
    if (i + 1) % width = 0 then printf "|\n")
;;

let compose =
  List.map2_exn ~f:(fun front back ->
    match front, back with
    | White, _ -> White
    | Black, _ -> Black
    | Transparent, p -> p)
;;

let f2 s =
  let layers = parse s in
  List.reduce_exn layers ~f:compose |> display;
  0
;;

let run () = Run.run ~f1 ~f2 Day08_input.data
