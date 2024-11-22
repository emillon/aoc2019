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

let initial_state initial_color =
  { colors = Map.singleton (module Vec) Vec.zero initial_color
  ; pos = Vec.zero
  ; dir = 0, 1
  }
;;

let rec robot s out_signal in_signal =
  let o = color_under_robot !s in
  Intcode.Signal.write out_signal (Color.to_int o);
  let paint_color = Color.of_int (Intcode.Signal.read in_signal) in
  let turn = Turn.of_int (Intcode.Signal.read in_signal) in
  s := next !s ~paint_color ~turn;
  robot s out_signal in_signal
;;

let interp c out_signal in_signal =
  let input () = Intcode.Signal.read in_signal in
  let output n = Intcode.Signal.write out_signal n in
  Intcode.eval_io_ c ~input ~output
;;

let run c initial_color =
  let code_to_robot = Intcode.Signal.create ~name:"code_to_robot" in
  let robot_to_code = Intcode.Signal.create ~name:"robot_to_code" in
  let s = ref (initial_state initial_color) in
  Intcode.Scheduler.run_both
    (fun () -> interp c code_to_robot robot_to_code)
    (fun () -> robot s robot_to_code code_to_robot);
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
