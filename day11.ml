module Color = struct
  type t =
    | White
    | Black
  [@@deriving sexp]

  let of_int = function
    | 0 -> Black
    | 1 -> White
    | n -> raise_s [%message "Color.of_int" (n : int)]
  ;;

  let to_int = function
    | Black -> 0
    | White -> 1
  ;;
end

module Turn = struct
  type t =
    | Left
    | Right
  [@@deriving sexp]

  let of_int = function
    | 0 -> Left
    | 1 -> Right
    | n -> raise_s [%message "Turn.of_int" (n : int)]
  ;;

  let to_vec = function
    | Left -> 0, 1
    | Right -> 0, -1
  ;;
end

let rotate dir turn = Vec.cmul dir (Turn.to_vec turn)

type state =
  { colors : Color.t Map.M(Vec).t
  ; pos : Vec.t
  ; dir : Vec.t
  }
[@@deriving sexp]

let color_at s p = Map.find s.colors p |> Option.value ~default:Black
let color_under_robot s = color_at s s.pos

let next s ~paint_color ~turn =
  let colors = Map.set s.colors ~key:s.pos ~data:paint_color in
  let dir = rotate s.dir turn in
  let pos = Vec.add s.pos dir in
  { colors; dir; pos }
;;

let total state = Map.length state.colors

module Protocol = struct
  type t =
    | Wait_paint_color
    | Wait_direction of { paint_color : Color.t }
    | Reply of Color.t
  [@@deriving sexp]

  let input = function
    | Reply c -> Color.to_int c, Wait_paint_color
    | t -> raise_s [%message "input" (t : t)]
  ;;

  let output t s n =
    match t with
    | Wait_paint_color ->
      let paint_color = Color.of_int n in
      Wait_direction { paint_color }, s
    | Wait_direction { paint_color } ->
      let turn = Turn.of_int n in
      let s' = next s ~paint_color ~turn in
      let o = color_under_robot s' in
      Reply o, s'
    | _ -> raise_s [%message "output" (t : t) (n : int)]
  ;;
end

let run c initial_color =
  let protocol = ref (Protocol.Reply initial_color) in
  let s = ref { colors = Map.empty (module Vec); pos = 0, 0; dir = 0, 1 } in
  let input () =
    let n, p' = Protocol.input !protocol in
    protocol := p';
    n
  in
  let output n =
    let p', s' = Protocol.output !protocol !s n in
    protocol := p';
    s := s'
  in
  Intcode.eval_io_ c ~input ~output;
  !s
;;

let f1 s = run (Intcode.parse s) Black |> total

let display s =
  let { Vec.min = xmin, ymin; max = xmax, ymax } = Vec.bounding_box_map s.colors in
  for y = ymax downto ymin do
    printf "|";
    for x = xmin to xmax do
      let s =
        match color_at s (x, y) with
        | Black -> " "
        | White -> "▉"
      in
      printf "%s" s
    done;
    printf "|\n"
  done
;;

let f2 s =
  run (Intcode.parse s) White |> display;
  0
;;

let run () = Run.run ~f1 ~f2 Day11_input.data
