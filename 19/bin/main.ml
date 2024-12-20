open Parsing

let parse_input input = 
  let available = input.(0)
    |> split_list (parse_word ", ")
    |> List.map fst
  in

  let patterns = Array.sub input 2 (Array.length input - 2) in
  available, patterns

let is_prefix pref word = pref |> String.to_seqi |> Seq.for_all (function i,c -> try c = word.[i] with _ -> false)

let possible =  
  let aux g (available,pattern,a) = if String.length pattern = 0 then 1 else match a with
  | [] -> 0
  | towel::t when is_prefix towel pattern ->
    let len_pattern = String.length pattern in
    let len_towel = String.length towel in 
    g (available, String.sub pattern len_towel (len_pattern - len_towel), available) + g (available,pattern,t)
  | _::t -> g (available,pattern,t)
  in
  let f = Memoisation.memoise_rec aux in

  fun pattern available -> f (available,pattern,available)

let ex1 input = 
  let available, patterns = parse_input input in

  patterns
  |> Array.fold_left (fun acc pattern -> acc + if possible pattern available <> 0 then 1 else 0) 0
let ex2 input = 
  let available, patterns = parse_input input in

  patterns
  |> Array.fold_left (fun acc pattern -> acc + possible pattern available) 0

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"