open Parsing

(* Key pads *)
let numeric_pad = [|
    "789" ;
    "456" ;
    "123" ;
    ".0A"
  |]
  |> Map.of_strings (function '.' -> raise Map.Out_of_bounds | c -> c)

let directional_pad = [|
  ".^A" ;
  "<v>"
|]
|> Map.of_strings (function '.' -> raise Map.Out_of_bounds | c -> c)

(* Construction of a automaton grah *)

let button_to_dir = function 
| '^' -> Map.top
| 'v' -> Map.down
| '<' -> Map.left
| '>' -> Map.right 
| _ -> invalid_arg "button_to_dir: Not a direction button"

let rec press state = 
  if String.length state = 0 then function _ -> state
  else function
  | 'A' -> 
    let first = state.[0] in
    let rest = String.sub state 1 (String.length state - 1) in
    Printf.sprintf "%c%s" first (press rest first)
  | butt ->
    let dir = button_to_dir butt in
    let rest = String.sub state 1 (String.length state - 1) in
    let cur = state.[0] in
    let map = if String.length state = 1 then numeric_pad else directional_pad in
    
    let pos = Map.find map (fun _ b -> b = cur) |> fst in
    
    Printf.sprintf "%c%s" (Map.get map (Map.step pos dir)) rest

let neighbors state = (* Assumes the state is not null *)
  
  directional_pad
  |> Map.fold (fun acc _ button -> button::acc) []
  |> List.filter_map (function button ->  try Some(press state button) with Map.Out_of_bounds ->  None)
  

let automaton = 
  let graph = Graph.create () in
  Graph.add_with_fun graph neighbors;

  graph


let output_char_state nb_dir_pads c = 
  Printf.sprintf "%s%c" (String.make (nb_dir_pads-1) 'A') c

let code_to_sequence code = code
  |> Array.to_seq
  |> Seq.fold_lefti (fun acc i c -> if i=0 then acc else
    (code.(i-1), c)::acc) []
  |> List.rev

let h state1 state2 = 
  let last = String.length state1 - 1 in
  let s1 = state1 |> String.to_seqi in
  let s2 = state2 |> String.to_seq in

  Seq.map2 (fun (i,c1) c2 -> ((if i = last then numeric_pad else directional_pad),c1,c2)) s1 s2
  |> Seq.fold_left (fun acc (pad,c1,c2) -> 
    let pos1 = Map.find pad (fun _ c -> c = c1) |> fst in
    let pos2 = Map.find pad (fun _ c -> c = c2) |> fst in
    
    acc + (Map.distance pos1 pos2)
    ) 0

let shortest_seq graph nb_dir_pads code = code
  |> String.to_seq
  |> Seq.map (output_char_state nb_dir_pads)
  |> Seq.cons (String.make nb_dir_pads 'A')
  |> Array.of_seq
  |> code_to_sequence
  |> List.fold_left (fun path (start, finish) -> 
    let new_path = Graph.astar h graph start finish in
    path @ new_path 
    ) []

let complexity graph nb_dir_pads code = 
  let sequence = shortest_seq graph nb_dir_pads code in
  List.length sequence * Scanf.sscanf code "%dA" (fun i -> i)

let ex1 input = 
  let nb_dir_pads = 3 in
  Printf.eprintf "Building ... %!" ;
  let graph = automaton in
  Printf.eprintf "Done\n%!";


  Array.to_seq input
  |> Seq.map (function code -> complexity graph nb_dir_pads code)
  |> Seq.fold_left (+) 0

let ex2 input =
  let nb_dir_pads = 6 in
  let graph = automaton in

  Array.to_seq input
  |> Seq.map (function code -> complexity graph nb_dir_pads code)
  |> Seq.fold_left (+) 0

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n%!";
  input |> ex2 |> Printf.printf "Result 2: %d\n%!"