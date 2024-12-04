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

type kind =
  | KPos
  | KImm
  | KRel

let mode_l n =
  let rec go n =
    let q = n / 10 in
    let r = n % 10 in
    let k =
      match r with
      | 0 -> KPos
      | 1 -> KImm
      | 2 -> KRel
      | _ -> raise_s [%message "mode_l" (r : int)]
    in
    if q = 0 then [ k ] else k :: go q
  in
  let pad l =
    match l with
    | [ _; _; _ ] -> l
    | [ _; _ ] -> KPos :: l
    | [ _ ] -> KPos :: KPos :: l
    | _ -> assert false
  in
  go n |> List.rev |> pad
;;

let get_op m ip mode_l i =
  assert (i <= max_param_count);
  match List.nth_exn mode_l (max_param_count - i) with
  | KPos -> Pos m.%{ip + i}
  | KImm -> Imm m.%{ip + i}
  | KRel -> Rel m.%{ip + i}
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
  let mode_l = mode_l modes in
  let op i = get_op t.mem t.ip mode_l i in
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

module Effect = Stdlib.Effect

module Conc = struct
  type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += Stop : unit Effect.t

  let fork f = Effect.perform (Fork f)
  let yield () = Effect.perform Yield
  let stop () = Effect.perform Stop
end

module Signal = struct
  type t =
    { name : string [@warning "-69"]
    ; values : int Queue.t
    }

  let create ~name = { name; values = Queue.create () }
  let debug = false

  let rec read var =
    match Queue.dequeue var.values with
    | None ->
      if debug then print_s [%message "read_blocked" (var.name : string)];
      Conc.yield ();
      read var
    | Some v ->
      if debug then print_s [%message "read_unblocked" (var.name : string) (v : int)];
      v
  ;;

  let write var x =
    if debug
    then print_s [%message "write" (var.name : string) (var.values : int Queue.t)];
    Queue.enqueue var.values x
  ;;
end

module Scheduler = struct
  let run_yield (main : unit -> unit) : unit =
    let run_q = Queue.create () in
    let enqueue k v =
      let task () = Effect.Deep.continue k v in
      Queue.enqueue run_q task
    in
    let dequeue () =
      match Queue.dequeue run_q with
      | None -> () (* done *)
      | Some task -> task ()
    in
    let rec spawn (f : unit -> unit) : unit =
      Effect.Deep.match_with
        f
        ()
        { retc = dequeue
        ; exnc = raise
        ; effc =
            (fun (type a) (eff : a Effect.t) ->
              match eff with
              | Conc.Yield ->
                Some
                  (fun (k : (a, unit) Effect.Deep.continuation) ->
                    enqueue k ();
                    dequeue ())
              | Conc.Fork f ->
                Some
                  (fun (k : (a, unit) Effect.Deep.continuation) ->
                    enqueue k ();
                    spawn f)
              | Conc.Stop -> Some (fun _ -> ())
              | _ -> None)
        }
    in
    spawn main
  ;;

  let run_both (a : unit -> unit) (b : unit -> unit) =
    run_yield (fun () ->
      Conc.fork (fun () ->
        a ();
        Conc.stop ());
      b ())
  ;;
end

let eval_scheduler c ~in_signal ~out_signal =
  let input () = Signal.read in_signal in
  let output n = Signal.write out_signal n in
  eval_io_ c ~input ~output
;;
