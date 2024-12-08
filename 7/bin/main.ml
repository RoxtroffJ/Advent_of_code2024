open Parsing

let optionnify_op op ao b = match ao with
| None -> Some b
| Some a -> Some (op a b)

let parse_line line = 
  let (test_val,_), rest = line |> (parse_int +> parse_word ": ") in

  let numbers = 
    rest
    |> split_list (parse_word " ")
    |> List.map
      (fun (s,_) -> parse_int s |> fst)
  in

  test_val, numbers

let valid o_operators (test_val, numbers) = 
  let rec aux result numbers = match numbers with
  | [] -> result = Some(test_val)
  | n::t -> List.fold_left (fun acc op -> acc || aux (op result n) t) false o_operators
  in

  aux None numbers

let ex1 operators input = 
  let o_operators = List.map optionnify_op operators in
  input
  |> Array.fold_left
    (fun acc line -> acc + 
      let p_line =  line |> parse_line in
      if valid o_operators p_line 
        then p_line |> fst
        else 0 
    )
    0 


let ex2 input = 
  let cat a b = 
    string_of_int a ^ string_of_int b
    |> int_of_string
  in

  ex1 [( + ); ( * ); cat] input

let _ =
  let input = "input" |> load in

  input |> (ex1 [( + ); ( * )]) |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"