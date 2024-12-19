open Parsing

(* Bytes are represented by Int. Programm is an array of Int. *)

let parse_input input = 
  let reg_A = Scanf.sscanf input.(0) "Register A: %d" (fun i -> i) in
  let reg_B = Scanf.sscanf input.(1) "Register B: %d" (fun i -> i) in
  let reg_C = Scanf.sscanf input.(2) "Register C: %d" (fun i -> i) in

  let prog = 
    input.(4)
    |> parse_word "Program: " |> snd
    |> split_list (parse_word ",")
    |> List.to_seq
    |> Seq.map (function s,_ -> Scanf.sscanf s "%d" (fun i -> i))
    |> Array.of_seq
  in

  reg_A, reg_B, reg_C, prog

type computer = {
  registers: int array;
  program: int array;
  mutable instr_pointer: int;
  output: int Dynarray.t
}

let create_computer reg_A reg_B reg_C prog = {
  registers = [|reg_A ; reg_B ; reg_C|];
  program = prog;
  instr_pointer = 0;
  output = Dynarray.create ()
}

let litteral_operand computer = 
  computer
    .program
    .(computer.instr_pointer + 1)

let combo_operand computer = 
  
  let reg = computer.registers in

  match litteral_operand computer with
  | v when v <= 3 -> v
  | 4 -> reg.(0)
  | 5 -> reg.(1)
  | 6 -> reg.(2)
  | _ -> invalid_arg "combo_operand (computer.program)"

let output_to_string computer = 

  let res = computer.output
  |> Dynarray.fold_left (fun acc i -> acc ^ string_of_int i ^ ",") ""
  in

  String.sub res 0 (String.length res - 1)

let step computer = 
  let prog = computer.program in
  let reg = computer.registers in
  let i = computer.instr_pointer in

  match prog.(i) with
  | 0 -> (* adv *)
    let param = combo_operand computer in
    let num = reg.(0) in
    
    let res = 
      if Int.shift_right_logical num param < 1 then 0
      else num / Int.shift_left 1 param
    in

    reg.(0) <- res;
    computer.instr_pointer <- i + 2

  | 1 -> (* bxl *)
    let num = reg.(1) in
    let param = litteral_operand computer in

    reg.(1) <- Int.logxor num param;
    computer.instr_pointer <- i + 2

  | 2 -> (* bst *) 
    let param = combo_operand computer in
    reg.(1) <- param mod 8;
    computer.instr_pointer <- i + 2

  | 3 -> (* jnz *)
    if reg.(0) = 0 then computer.instr_pointer <- i + 2 
    else computer.instr_pointer <- litteral_operand computer

  | 4 -> (* bxc *)
    let _param = litteral_operand computer in
    let res = Int.logxor reg.(1) reg.(2) in
    reg.(1) <- res;
    computer.instr_pointer <- i + 2

  | 5 -> (* out *)
    let param = combo_operand computer in
    Dynarray.add_last computer.output (param mod 8);
    computer.instr_pointer <- i + 2

  | 6 -> (* bdv *)
    let param = combo_operand computer in
    let num = reg.(0) in
    
    let res = 
      if Int.shift_right_logical num param < 1 then 0
      else num / Int.shift_left 1 param
    in

    reg.(1) <- res;
    computer.instr_pointer <- i + 2
  
  | 7 -> (* cdv *)
    let param = combo_operand computer in
    let num = reg.(0) in
    
    let res = 
      if Int.shift_right_logical num param < 1 then 0
      else num / Int.shift_left 1 param
    in

    reg.(2) <- res;
    computer.instr_pointer <- i + 2
    
  | _ -> invalid_arg "step (computer.program)"

let rec run computer = try
  step computer;
  run computer
with _ -> ()


let ex1 input = 
  let reg_A, reg_B, reg_C, prog = parse_input input in
  let computer = create_computer reg_A reg_B reg_C prog in

  run computer;

  output_to_string computer

let find prog acc reg_B reg_C target = 
  let reg_A_acc = Int.shift_left acc 3 in
  let rec aux i = 
    if i > 7 then [] else begin
    let reg_A = reg_A_acc + i in
    let computer = create_computer reg_A reg_B reg_C prog in
    
    run computer;

    if Dynarray.get computer.output 0 = target then (reg_A)::aux (i+1)
    else aux (i+1)
  end in

  aux 0

let ex2 input = 
  let _reg_A, reg_B, reg_C, prog = parse_input input in
  
  let exception Done of int in

  let rec aux acc i =
    let target = try prog.(i) with _ -> raise (Done acc) in 

    for _ = 0 to i-1 do
      Printf.eprintf "\t";
    done; Printf.eprintf "Looking for %d ... " target;

    let l = find prog acc reg_B reg_C target in

    Printf.eprintf "Found %d solutions\n" (List.length l);

    List.iter (function reg_A -> 
      aux reg_A (i-1)) l

  in

  try aux 0 (Array.length prog - 1) ; -1 with Done acc -> acc 

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %s\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"