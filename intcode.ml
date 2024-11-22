type t = int Map.M(Int).t

let parse s =
  String.strip s
  |> String.split ~on:','
  |> List.mapi ~f:(fun i s -> i, Int.of_string s)
  |> Map.of_alist_exn (module Int)
;;

let ( .%{} ) m a =
  match Map.find m a with
  | Some v -> v
  | None ->
    assert (a >= 0);
    0
;;

let ( .%{}<- ) m key data = Map.set m ~key ~data

type operand =
  | Pos of int
  | Imm of int
  | Rel of int
[@@deriving sexp]

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
  | Rel_base_offset of { offset : operand }

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
  | '2' -> Rel m.%{ip + i}
  | _ -> raise_s [%message "op mode" (op_mode : char)]
;;

type state =
  { mem : t
  ; ip : int
  ; rel_base : int
  }

type operand_value =
  | Value of int
  | Address of int

let operand_value t = function
  | Imm x -> Value x
  | Pos x -> Address x
  | Rel x -> Address (t.rel_base + x)
;;

let effective_address t op =
  match operand_value t op with
  | Address x -> x
  | Value _ -> assert false
;;

let decode t =
  let low, modes = decode_opcode t.mem.%{t.ip} in
  let op i = get_op t.mem t.ip modes i in
  match low with
  | 1 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> effective_address t in
    Add { src1; src2; adst }, 4
  | 2 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> effective_address t in
    Mul { src1; src2; adst }, 4
  | 3 ->
    let adst = op 1 |> effective_address t in
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
    let adst = op 3 |> effective_address t in
    Lt { src1; src2; adst }, 4
  | 8 ->
    let src1 = op 1 in
    let src2 = op 2 in
    let adst = op 3 |> effective_address t in
    Eq { src1; src2; adst }, 4
  | 9 ->
    let offset = op 1 in
    Rel_base_offset { offset }, 2
  | 99 -> Hlt, 1
  | opcode -> raise_s [%message "decode" (opcode : int)]
;;

let ( .%%{} ) t op =
  match operand_value t op with
  | Value v -> v
  | Address p -> t.mem.%{p}
;;

type k =
  | Interpret of state
  | Input of (int -> k)
  | Output of int * k
  | Halted

let interpret_step t =
  match decode t with
  | Add { src1; src2; adst }, len ->
    let t' = { t with mem = t.mem.%{adst} <- t.%%{src1} + t.%%{src2}; ip = t.ip + len } in
    Interpret t'
  | Mul { src1; src2; adst }, len ->
    let t' = { t with mem = t.mem.%{adst} <- t.%%{src1} * t.%%{src2}; ip = t.ip + len } in
    Interpret t'
  | Input { adst }, len ->
    Input
      (fun v ->
        let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
        Interpret t')
  | Output { src }, len ->
    Output
      ( t.%%{src}
      , let t' = { t with ip = t.ip + len } in
        Interpret t' )
  | Hlt, _ -> Halted
  | Jnz { src; target }, len ->
    let ip = if t.%%{src} <> 0 then t.%%{target} else t.ip + len in
    let t' = { t with ip } in
    Interpret t'
  | Jz { src; target }, len ->
    let ip = if t.%%{src} = 0 then t.%%{target} else t.ip + len in
    let t' = { t with ip } in
    Interpret t'
  | Lt { src1; src2; adst }, len ->
    let v = if t.%%{src1} < t.%%{src2} then 1 else 0 in
    let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
    Interpret t'
  | Eq { src1; src2; adst }, len ->
    let v = if t.%%{src1} = t.%%{src2} then 1 else 0 in
    let t' = { t with mem = t.mem.%{adst} <- v; ip = t.ip + len } in
    Interpret t'
  | Rel_base_offset { offset }, len ->
    let n = t.%%{offset} in
    let t' = { t with ip = t.ip + len; rel_base = t.rel_base + n } in
    Interpret t'
;;

let rec interpret t ~input ~output =
  let rec interpret_k = function
    | Interpret t' -> interpret ~input ~output t'
    | Input f -> interpret_k (f (input ()))
    | Output (v, k) ->
      output v;
      interpret_k k
    | Halted -> t.mem
  in
  interpret_k (interpret_step t)
;;

let create_state mem = { mem; ip = 0; rel_base = 0 }

let eval mem =
  let input _ = failwith "no input function" in
  let output _ = failwith "no output function" in
  let m = interpret ~input ~output (create_state mem) in
  Map.find_exn m 0
;;

let eval_io mem ~input ~output = interpret ~input ~output (create_state mem)

let eval_io_ mem ~input ~output =
  let _ : t = eval_io mem ~input ~output in
  ()
;;
