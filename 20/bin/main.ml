open Parsing

type cell = Wall | Start | End of int | Track of int

let directions = [Map.top ; Map.down ; Map.left ; Map.right]

let find_neighbor f map pos = 
  List.find (function dir -> try
    let p = Map.step pos dir in 
    let cell = try Map.get map p with Map.Out_of_bounds -> raise Exit in
    f cell
  with Exit -> false) directions
  |> Map.step pos

let map_of_input input = 
  let map = Map.of_strings (
    function 
    | '.' -> Track (-1) 
    | '#' -> Wall 
    | 'S' -> Start 
    | 'E' -> End (-1)
    | _ -> invalid_arg "map_of_input"
  ) input in

  let start = Map.find map (function _ -> function Start -> true | _ -> false) |> fst in
  let finish = Map.find map (function _ -> function End _ -> true | _ -> false) |> fst in

  let rec measure pos time = if pos = finish then Map.set map pos (End time) else begin
    if pos <> start then Map.set map pos (Track time);
    let next = find_neighbor (function End _ | Track (-1) -> true | _ -> false) map pos in

    measure next (time+1)
  end in
  
  measure start 0;
  map, start, finish

let step map pos = 
  let i = match Map.get map pos with | Track i -> i | Start -> 0 | _ -> invalid_arg "step (not on the tracks or at the end)" in
  find_neighbor (function End j | Track j when j = i+1 -> true | _ -> false) map pos

let rec track map start = match Map.get map start with
| End _ -> []
| Track _ | Start -> start :: track map (step map start)
| Wall -> invalid_arg "track (start position is in a wall)"

let get_time map pos = match Map.get map pos with
| Start -> 0
| Track i | End i -> i
| Wall -> invalid_arg "get_time (called on a wall)"

let ex1 input = 
  let map, start, _finish = map_of_input input in
  let tr = track map start in

  let compute_cheats pos = 
    directions
    |> List.map (function dir -> Map.step (Map.step pos dir) dir)
    |> List.filter_map (function new_pos -> try Some (get_time map new_pos - get_time map pos - 2) with _ -> None)
  in

  tr
  |> List.fold_left (fun acc pos -> 
    compute_cheats pos 
    |> List.filter ((<=) 100)
    |> List.length
    |> (+) acc) 0

let ex2 input = 
  let map, start, _finish = map_of_input input in
  let tr = track map start in

  let all_pos = map |> Map.fold (fun acc new_pos _ -> new_pos::acc) [] in

  let compute_cheats pos = 
    all_pos
    |> List.filter_map (function new_pos ->
      let dist = Map.distance pos new_pos in
      if dist > 20 then None else
      try Some (get_time map new_pos - get_time map pos - dist) with _ -> None)
  in

  tr
  |> List.fold_left (fun acc pos -> 
    compute_cheats pos 
    |> List.filter ((<=) 100)
    |> List.length
    |> (+) acc) 0

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"