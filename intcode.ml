type t = int Map.M(Int).t

let parse s =
  String.strip s
  |> String.split ~on:','
  |> List.mapi ~f:(fun i s -> i, Int.of_string s)
  |> Map.of_alist_exn (module Int)
;;

let ( .%{} ) m a = Map.find_exn m a
let ( .%{}<- ) m key data = Map.set m ~key ~data

type operand =
  | Pos of int
  | Imm of int

type instr =
  | Add of
      { src1 : operand
      ; src2 : operand
      ; adst : int
      }
  | Mul of
      { src1 : operand
      ; src2 : operand
      ; adst : int
      }
  | Input of { adst : int }
  | Output of { src : operand }
  | Hlt
  | Jnz of
      { src : operand
      ; target : operand
      }
  | Jz of
      { src : operand
      ; target : operand
      }
  | Lt of
      { src1 : operand
      ; src2 : operand
      ; adst : int
      }
  | Eq of
      { src1 : operand
      ; src2 : operand
      ; adst : int
      }

let decode_opcode n =
  let low = n % 100 in
  let modes = n / 100 in
  low, modes
;;

let max_param_count = 3

let get_op m ip modes i =
  assert (i <= max_param_count);
  let s = Printf.sprintf "%0*d" max_param_count modes |> String.rev in
  let op_mode = s.[i - 1] in
  match op_mode with
  | '0' -> Pos m.%{ip + i}
  | '1' -> Imm m.%{ip + i}
  | _ -> raise_s [%message "op mode" (op_mode : char)]
;;

let as_pos = function
  | Pos x -> x
  | Imm _ -> assert false
;;

let decode m ip =
  let low, modes = decode_opcode m.%{ip} in
  let op i = get_op m ip modes i in
  match low with
  | 1 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> as_pos in
    Add { src1; src2; adst }, 4
  | 2 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> as_pos in
    Mul { src1; src2; adst }, 4
  | 3 ->
    let adst = op 1 |> as_pos in
    Input { adst }, 2
  | 4 ->
    let src = op 1 in
    Output { src }, 2
  | 5 ->
    let src = op 1 in
    let target = op 2 in
    Jnz { src; target }, 3
  | 6 ->
    let src = op 1 in
    let target = op 2 in
    Jz { src; target }, 3
  | 7 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> as_pos in
    Lt { src1; src2; adst }, 4
  | 8 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> as_pos in
    Eq { src1; src2; adst }, 4
  | 99 -> Hlt, 1
  | opcode -> raise_s [%message "decode" (opcode : int)]
;;

type state =
  { mem : t
  ; ip : int
  ; input : unit -> int
  ; output : int -> unit
  }

let ( .%%{} ) t = function
  | Pos p -> t.mem.%{p}
  | Imm v -> v
;;

let rec interpret t =
  match decode t.mem t.ip with
  | Add { src1; src2; adst }, len ->
    let t' = { t with mem = t.mem.%{adst} <- t.%%{src1} + t.%%{src2}; ip = t.ip + len } in
    interpret t'
  | Mul { src1; src2; adst }, len ->
    let t' = { t with mem = t.mem.%{adst} <- t.%%{src1} * t.%%{src2}; ip = t.ip + len } in
    interpret t'
  | Input { adst }, len ->
    let v = t.input () in
    let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
    interpret t'
  | Output { src }, len ->
    t.output t.%%{src};
    let t' = { t with ip = t.ip + len } in
    interpret t'
  | Hlt, _ -> t.mem
  | Jnz { src; target }, len ->
    let ip = if t.%%{src} <> 0 then t.%%{target} else t.ip + len in
    let t' = { t with ip } in
    interpret t'
  | Jz { src; target }, len ->
    let ip = if t.%%{src} = 0 then t.%%{target} else t.ip + len in
    let t' = { t with ip } in
    interpret t'
  | Lt { src1; src2; adst }, len ->
    let v = if t.%%{src1} < t.%%{src2} then 1 else 0 in
    let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
    interpret t'
  | Eq { src1; src2; adst }, len ->
    let v = if t.%%{src1} = t.%%{src2} then 1 else 0 in
    let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
    interpret t'
;;

let eval mem =
  let m =
    interpret
      { mem
      ; input = (fun _ -> failwith "no input function")
      ; output = (fun _ -> failwith "no output function")
      ; ip = 0
      }
  in
  Map.find_exn m 0
;;

let eval_io mem ~input:input_value ~output =
  let input_done = ref false in
  let input () =
    assert (not !input_done);
    input_done := true;
    input_value
  in
  let _ : t = interpret { mem; ip = 0; input; output } in
  ()
;;
