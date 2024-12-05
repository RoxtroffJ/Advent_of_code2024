open Parsing

let _ =
  let input = "example" |> load in
  
  let ex1 = 0 in
  let ex2 = 0 in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2