let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_using
    (let+ min = number <* char '-'
     and+ max = number <* end_of_line in
     List.range min max ~stop:`inclusive)
;;

let digits n = Int.to_string n |> String.to_list

let has_group_of_2 l ~strict =
  List.group l ~break:Char.( <> )
  |> List.exists ~f:(fun g ->
    let l = List.length g in
    if strict then l = 2 else l >= 2)
;;

let is_ok n ~strict =
  let d = digits n in
  List.is_sorted d ~compare:Char.compare && has_group_of_2 d ~strict
;;

let f1 s = parse s |> List.count ~f:(is_ok ~strict:false)
let f2 s = parse s |> List.count ~f:(is_ok ~strict:true)
let run () = Run.run ~f1 ~f2 Day04_input.data
