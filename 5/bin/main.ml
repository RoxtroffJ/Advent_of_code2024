open Parsing

let _ =
  let file_prefix = "input" in
  let order_suffix = "_order" in

  let input = file_prefix |> load in
  let order = file_prefix ^ order_suffix |> load in
  
  let cmp = 
    (* Hash table of ordering rules *)
    let tbl = Hashtbl.create 42 in
    order |> Array.iter (
      function line -> 
        let (a,_),b = line |> parse_int +> parse_word "|" +> parse_int |> fst in
        Printf.eprintf "%d|%d\n" a b ; Hashtbl.add tbl a b 
      ) ;
    fun a b ->
      if a = b then 0
      else 
        let rec search = function
        | [] -> 1
        | h::_ when h = b -> -1
        | _::t -> search t
          (* if search t == -1 then -1 *)
          (* else Hashtbl.find_all tbl h |> search *)
        in
        Hashtbl.find_all tbl a |> search
  in
  
  let ex1 = 
    let parsed_input = 
      input 
      |> Array.map (split_list (parse_word ","))
      |> Array.map (List.map fst)
      |> Array.map (List.map parse_int)
      |> Array.map (List.map fst)
    in
    parsed_input 
    |> Array.fold_left
      (fun acc line ->
        acc + begin
          let sorted_line = List.sort cmp line in
          Printf.eprintf "\nOriginal :" ; List.iter (Printf.eprintf "%d ") line;
          Printf.eprintf "\nSorted   :" ; List.iter (Printf.eprintf "%d ") sorted_line;
          Printf.eprintf "\n";
          if line = sorted_line then (
            let a = Array.of_list line in
            let len = Array.length a in
            let res = a.(len/2) in
            Printf.eprintf "Found %d\n" res;
            res
          )
          else 0
        end
      )
      0
  in
  let ex2 = 
    let parsed_input = 
      input 
      |> Array.map (split_list (parse_word ","))
      |> Array.map (List.map fst)
      |> Array.map (List.map parse_int)
      |> Array.map (List.map fst)
    in
    parsed_input 
    |> Array.fold_left
      (fun acc line ->
        acc + begin
          let sorted_line = List.sort cmp line in
          Printf.eprintf "\nOriginal :" ; List.iter (Printf.eprintf "%d ") line;
          Printf.eprintf "\nSorted   :" ; List.iter (Printf.eprintf "%d ") sorted_line;
          Printf.eprintf "\n";
          if not (line = sorted_line) then (
            let a = Array.of_list sorted_line in
            let len = Array.length a in
            let res = a.(len/2) in
            Printf.eprintf "Found %d\n" res;
            res
          )
          else 0
        end
      )
      0
  in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2