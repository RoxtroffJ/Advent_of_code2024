open Parsing

type square = Wall | Path | Start | Finish

let square_of_char = function 
| '#' -> Wall
| '.' -> Path
| 'S' -> Start
| 'E' -> Finish
| _ -> invalid_arg "square_of_char"


module Prio = Prio_queue.Make(Int)

let ex1 input = 
  let map = Map.of_strings square_of_char input in
  let start_pos = Map.find map (fun _ s -> s = Start) |> fst in
  let end_pos = Map.find map (fun _ s -> s = Finish) |> fst in

  (* A* algorithm *)
  let queue = Prio.create true in
  let seen = Hashtbl.create 42 in

  let push (pos, dir, g) = Prio.upgrade queue (pos,dir) (g + Map.distance pos end_pos) |> ignore in

  let rec astar () = 
    let (pos,dir), f = Prio.pop queue in

    if Hashtbl.mem seen (pos,dir) then astar () else begin
    
    Hashtbl.add seen (pos,dir) ();
    
    let g = f - Map.distance pos end_pos in
    if pos = end_pos then g
    
    else begin
      (* compute the neighbors *)
      let neighbors = [
        pos, Map.rot_clockwise dir, g + 1000 ;
        pos, Map.rot_anticlockwise dir, g + 1000 ;
        Map.step pos dir, dir, g + 1
      ] in
      neighbors
      |> List.filter (function pos,_,_ -> match Map.get map pos with Wall -> false | _ -> true)
      |> List.filter (function pos,dir,_ -> Hashtbl.mem seen (pos,dir) |> not)
      |> List.iter push;
      astar ()
    end end
  
  in 
  push (start_pos,Map.right,0);
  astar ()


let ex2 input =

  let map = Map.of_strings square_of_char input in
  let start_pos = Map.find map (fun _ s -> s = Start) |> fst in
  let end_pos = Map.find map (fun _ s -> s = Finish) |> fst in

  (* tracking A* algorithm *)
  let queue = Prio.create true in
  let seen = Hashtbl.create 42 in

  let predecessors = Hashtbl.create 42 in

  let push (prev, pos, dir, g) = if Prio.upgrade queue (pos,dir) (g + Map.distance pos end_pos) then Hashtbl.add predecessors (pos,dir) (g,prev) in

  let success_score = ex1 input in

  let rec astar () = try
    let (pos,dir), f = Prio.pop queue in
    if f > success_score then () else begin
    if Hashtbl.mem seen (pos,dir) then astar () else begin
    
    Hashtbl.add seen (pos,dir) ();
    
    let g = f - Map.distance pos end_pos in
    if pos = end_pos then astar ()
    
    else begin
      (* compute the neighbors *)
      let neighbors = [
        Some(pos,dir), pos, Map.rot_clockwise dir, g + 1000 ;
        Some(pos,dir), pos, Map.rot_anticlockwise dir, g + 1000 ;
        Some(pos,dir), Map.step pos dir, dir, g + 1
      ] in
      neighbors
      |> List.filter (function _,pos,_,_ -> match Map.get map pos with Wall -> false | _ -> true)
      |> List.iter push;
      astar ()
    end end end
  with Prio.Empty -> ()
  
  in 
  push (None,start_pos,Map.right,0);
  astar ();

  let mark = Hashtbl.create 42 in
  let rec reconstruct (pos,dir) = 
    Hashtbl.replace mark pos ();
    let prev = Hashtbl.find_all predecessors (pos,dir) in
    let g_min = List.fold_left (fun g_min (g,_) -> min g g_min) Int.max_int prev in
    prev 
    |> List.filter (function (g,_) -> g = g_min)
    |> List.filter_map (function (_,a) -> a)
    |> List.iter reconstruct
  in
  List.iter (function dir -> reconstruct (end_pos, dir)) [Map.top ; Map.down ; Map.left ; Map.right];
  Hashtbl.length mark

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"