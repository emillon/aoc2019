module Vec3 = struct
  type t = int * int * int [@@deriving compare, equal, hash, sexp]

  let zero = 0, 0, 0
  let l1_norm (x, y, z) = abs x + abs y + abs z
  let map2 ~f (ax, ay, az) (bx, by, bz) = f ax bx, f ay by, f az bz
  let add = map2 ~f:( + )
end

module Moon = struct
  type t =
    { pos : Vec3.t
    ; vel : Vec3.t
    }
  [@@deriving compare, equal, hash, sexp]

  let energy { pos; vel } = Vec3.l1_norm pos * Vec3.l1_norm vel
  let map_pos ~f t = { t with pos = f t.pos }
end

module State = struct
  type t = Moon.t list [@@deriving compare, hash, sexp]
end

let parse =
  let open Parsing_util in
  let open Angstrom in
  parse_lines_using
    (let+ x = string "<x=" *> signed_number
     and+ y = string ", y=" *> signed_number
     and+ z = string ", z=" *> signed_number <* string ">" in
     { Moon.pos = x, y, z; vel = Vec3.zero })
;;

let apply_gravity_for_one (moon : Moon.t) (other : Moon.t) =
  let acceleration = Vec3.map2 ~f:Int.compare other.pos moon.pos in
  { moon with vel = Vec3.add moon.vel acceleration }
;;

let apply_gravity t =
  List.map t ~f:(fun m -> List.fold t ~init:m ~f:apply_gravity_for_one)
;;

let apply_velocity_for (t : Moon.t) = { t with pos = Vec3.add t.pos t.vel }
let apply_velocity t = List.map ~f:apply_velocity_for t
let next t = t |> apply_gravity |> apply_velocity
let total_energy t = List.map t ~f:Moon.energy |> Algo.sum

let sample =
  String.concat_lines
    [ "<x=-1, y=0, z=2>"; "<x=2, y=-10, z=-7>"; "<x=4, y=-8, z=8>"; "<x=3, y=5, z=-1>" ]
;;

let%expect_test _ =
  let s = parse sample in
  s |> next |> [%sexp_of: State.t] |> print_s;
  [%expect
    {|
    (((pos (2 -1 1)) (vel (3 -1 -1))) ((pos (3 -7 -4)) (vel (1 3 3)))
     ((pos (1 -7 5)) (vel (-3 1 -3))) ((pos (2 2 0)) (vel (-1 -3 1))))
    |}];
  let after10 = s |> Fn.apply_n_times ~n:10 next in
  after10 |> [%sexp_of: State.t] |> print_s;
  [%expect
    {|
    (((pos (2 1 -3)) (vel (-3 -2 1))) ((pos (1 -8 0)) (vel (-1 1 3)))
     ((pos (3 -6 1)) (vel (3 2 -3))) ((pos (2 0 4)) (vel (1 -1 -1))))
    |}];
  total_energy after10 |> printf "%d";
  [%expect {| 179 |}]
;;

let f1 s = parse s |> Fn.apply_n_times ~n:1000 next |> total_energy

let period t0 =
  let r = ref t0 in
  let seen = Hash_set.of_list (module State) [ t0 ] in
  let exception Found of int in
  let i = ref 1 in
  try
    while true do
      let t = next !r in
      if Hash_set.mem seen t then raise (Found !i);
      Hash_set.add seen t;
      r := t;
      Int.incr i
    done
  with
  | Found n -> n
;;

let sample2 =
  String.concat_lines
    [ "<x=-8, y=-10, z=0>"; "<x=5, y=5, z=10>"; "<x=2, y=-7, z=3>"; "<x=9, y=-8, z=-3>" ]
;;

let to_x (x, _, _) = x, 0, 0
let to_y (_, y, _) = 0, y, 0
let to_z (_, _, z) = 0, 0, z

let f2 s =
  let t = parse s in
  [ to_x; to_y; to_z ]
  |> List.map ~f:(fun axis -> t |> List.map ~f:(Moon.map_pos ~f:axis) |> period)
  |> List.reduce_exn ~f:Algo.lcm
;;

let%expect_test "f2" =
  let test s = f2 s |> printf "%d" in
  test sample;
  [%expect {| 2772 |}];
  test sample2;
  [%expect {| 4686774924 |}]
;;

let run () = Run.run ~f1 ~f2 Day12_input.data
