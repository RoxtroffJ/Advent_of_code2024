open Parsing

type obj = Robot | Box | Wall | Empty | Box_left | Box_right

type map_line = obj array

type map = {
  robot_pos : int * int;
  map : map_line array
}

let top = -1,0
let left = 0,-1
let bottom = 1,0
let right = 0,1

let sting_of_direction dir = 
  if dir = left then "<"
  else if dir = right then ">"
  else if dir = top then "^"
  else "v"

let parse_map_line str = 
  if str = "" then ([||], str)
  else try match Scanf.sscanf str "%[.O#@]%n" (fun l s -> l,String.sub str s (String.length str - s)) with
  | "",_ -> raise Parsing_Failure
  | l,rest -> l
    |> String.to_seq
    |> Seq.map (function '.' -> Empty | 'O' -> Box | '#' -> Wall | '@' -> Robot | _ -> failwith "Impossible case  happened in parse_map_line")
    |> Array.of_seq, rest
  with _ -> raise Parsing_Failure

let parse_direction_line str = try match Scanf.sscanf str "%[<>v^]%n" (fun l s -> l,String.sub str s (String.length str - s)) with
  | "","" -> [||],""
  | "",_ -> raise Parsing_Failure
  | l,rest -> l
    |> String.to_seq
    |> Seq.map (function '^' -> top | 'v' -> bottom | '<' -> left | '>' -> right | _ -> failwith "Impossible case  happened in parse_direction_line")
    |> Array.of_seq, rest
with _ -> raise Parsing_Failure

let find_robot map = 
  let exception Found of int*int in
  try Array.to_seqi map.map
  |> Seq.map (function i,line -> i,Array.to_seqi line)
  |> Seq.iter (fun (i,line) -> 
    Seq.iter (fun (j,c) -> match c with
    | Robot -> Found (i,j) |> raise
    | _ -> ()
    ) line
  );
  raise Not_found
  with Found (i,j) -> {map with robot_pos = i,j}

let parse_input input = 
  let map = Dynarray.create () in
  let directions = Dynarray.create () in

  Array.iter (function line -> 
    try 
      let obj,_ = parse_map_line line in
      Dynarray.add_last map obj
    with Parsing_Failure ->
      let dir,_ = parse_direction_line line in
      Dynarray.append_array directions dir) input
  ;

  find_robot {
    robot_pos = 0,0;
    map = Dynarray.to_array map
  }, Dynarray.to_array directions

let step (i,j) (di,dj) = i+di,j+dj
let get matrix (i,j) = matrix.(i).(j)
let set matrix (i,j) e = matrix.(i).(j) <- e

let swap matrix pos1 pos2 = 
  let temp = get matrix pos1 in
  set matrix pos1 (get matrix pos2);
  set matrix pos2 temp

let rec move map pos dir = try  
  
  (* Check if we move the robot for map update *)
  let is_robot = match get map.map pos with 
  | Wall -> raise Exit 
  | Robot -> true
  | _ -> false
  in

  (* Finds all the front positions *)
  let current,front = match get map.map pos with
  | Box_left -> 
    let front = step pos dir in 
    if dir = left then [pos; step pos right],[front]
    else if dir = right then [step pos right; pos],[step front right]
    else [pos; step pos right],[front; step front right]
  | Box_right -> 
      let front = step pos dir in 
      if dir = left then [step pos left; pos],[step front left]
      else if dir = right then [pos ; step pos left],[front]
      else [pos; step pos left],[front; step front left]
  | _ -> [pos],[step pos dir] in
  
  (* Function to step forward and return the map *)
  let return map = let new_map = 
    Array.map Array.copy map.map in
    List.iter (fun pos -> 
      swap new_map pos (step pos dir)) current ; 
    {map = new_map ; robot_pos = if is_robot then List.hd front else map.robot_pos} in
  
  (* Computes a map where all the front squares are available *)
  let new_map = try Some(List.fold_left (fun map pos -> 
    match get map.map pos with
    | Empty -> map
    | _ -> 
      let new_map = move map pos dir in
      match get new_map.map pos with
      | Empty -> new_map
      | _ -> raise Exit) map front)
  with Exit -> None in

  match new_map with 
  | None -> raise Exit
  | Some(new_map) -> return new_map

with _ -> map

let gps map = 
  let gps_one i j c = match c with
  | Box | Box_left-> 100*i + j
  | _ -> 0
  in

  let gps_line (i,line) = 
    line 
    |> Array.to_seqi
    |> Seq.fold_left (fun acc (j,c) -> acc + gps_one i j c) 0
  in

  map.map
  |> Array.to_seqi
  |> Seq.fold_left (fun acc l -> acc + gps_line l) 0

let string_of_map map = 
  Array.fold_left (fun acc line ->
    acc ^ Array.fold_left (fun acc c -> acc ^ match c with Empty -> "." | Wall -> "#" | Robot -> "@" | Box -> "O" | Box_left -> "[" | Box_right -> "]") "" line ^ "\n") "" map.map
  
let big_map map = Printf.eprintf "From\n%s\n" (string_of_map map);
  let new_map = Array.make_matrix (Array.length map.map - 1) (2 * Array.length map.map.(0)) Empty in
  map.map
  |> Array.to_seqi
  |> Seq.map (function i,l -> i, Array.to_seqi l)
  |> Seq.iter (function i,l -> l |> Seq.iter (function j,p -> 
    match p with
    | Wall -> new_map.(i).(2*j) <- Wall ; new_map.(i).(2*j+1) <- Wall
    | Box -> new_map.(i).(2*j) <- Box_left ; new_map.(i).(2*j+1) <- Box_right
    | Robot -> new_map.(i).(2*j) <- Robot
    | _ -> ())
  );
  let res = {
    robot_pos = (let i,j = map.robot_pos in i,2*j);
    map = new_map
  } in
  Printf.eprintf "\nTo\n%s\n" (string_of_map res);
  res


let ex1 input = 
  let map,direction = parse_input input in
  let final_map = Array.fold_left (fun map dir -> 
    move map map.robot_pos dir) map direction in
  
  gps final_map

let ex2 input = 
  let map,direction = parse_input input in
  let final_map = Array.fold_left (fun map dir -> Printf.eprintf "\n%s\n%s\n" (map |> string_of_map) (dir |> sting_of_direction);
    move map map.robot_pos dir) (big_map map) direction in
  Printf.eprintf "\n%s\n" (final_map |> string_of_map);
  gps final_map

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"