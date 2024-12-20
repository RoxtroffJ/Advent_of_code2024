open Parsing

let ex1 _input = 0
let ex2 _input = 0

let _ =
  let input = "example" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"