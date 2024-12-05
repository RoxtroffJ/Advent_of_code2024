open Parsing

let _ =
  let input = "input" |> load in
  
  let ex1 = input |> Array.fold_left 
    (fun res text ->  
       find_all (parse_word "mul(" +> parse_int +> parse_word "," +> parse_int +> parse_word ")") text 
       |> List.fold_left (fun res ((((_,a),_),b),_) -> Printf.eprintf "%d,%d\n" a b ; res + a * b) res
    )
    0
  in 

  let ex2 = input 
    |> Array.to_list 
    |> String.concat "" 
    |> split_list (parse_word_res true "do()" <|> parse_word_res false "don't()") 
    |> List.fold_left 
      (fun res (text, activated) ->
          if Option.is_some activated && not (Option.get activated) then res
          else
            find_all (parse_word "mul(" +> parse_int +> parse_word "," +> parse_int +> parse_word ")") text
            |> List.fold_left (fun res ((((_,a),_),b),_) -> Printf.eprintf "%d,%d\n" a b ; res + a * b) res)
      0
  in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2

