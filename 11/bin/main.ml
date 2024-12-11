open Parsing
open Memoisation

let parse_input input = 
  split_list (parse_word " ") input.(0)
  |> List.map (fun (x,_) -> parse_int x |> fst)

let step_rock x = match x with
| 0 -> [1]
| _ -> 
  let str = string_of_int x in
  if String.length str mod 2 = 0 then [int_of_string (String.sub str 0 (String.length str / 2)); int_of_string (String.sub str (String.length str / 2) (String.length str / 2))]
  else [x*2024]

let step_n = 
  let rec_step_n rec_call (n,x) = 
    if n = 0 then 1
    else 
      x
      |> step_rock
      |> List.map (fun x -> rec_call (n-1,x))
      |> List.fold_left (+) 0
  in

  let aux = memoise_rec rec_step_n in
  fun n x -> aux (n,x)
  

(* let print_stones out stones = 
  stones |> List.iter (fun x -> Printf.fprintf out "%d " x);
  Printf.fprintf out "\n" *)

let ex input n = 
  let stones = parse_input input in

  stones |> List.map (step_n n) |> List.fold_left (+) 0


let ex1 input = ex input 25
let ex2 input = ex input 75

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"