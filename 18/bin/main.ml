open Parsing

let width,height = 70,70

type square = Safe | Corrupted

let parse_line line = Scanf.sscanf line "%d,%d" (fun x y -> Map.pos_of_int y x)
let parse_input = Array.map parse_line 



let ex1 input = 
  let corrupted = parse_input input in
  let map = Map.create_rectangle (height+1) (width+1) (function _ -> Safe) in

  Array.sub corrupted 0 1024
  |> Array.iter (function pos -> Map.set map pos Corrupted);

  (Map.path_find 
    (function Safe -> false | Corrupted -> true) 
    [Map.down ; Map.left ; Map.right ; Map.top]
    Map.distance
    map
    (Map.pos_of_int 0 0)
    (Map.pos_of_int height width)
  |> List.length) - 1


let ex2 input = 
  let corrupted = parse_input input in
  let map = Map.create_rectangle (height+1) (width+1) (function _ -> Safe) in

  let rec aux i = try
    Map.set map corrupted.(i) Corrupted;

    Map.path_find 
      (function Safe -> false | Corrupted -> true) 
      [Map.down ; Map.left ; Map.right ; Map.top]
      Map.distance
      map
      (Map.pos_of_int 0 0)
      (Map.pos_of_int height width)
    |> ignore;

    aux (i+1)
  with _ -> 
    let pos = corrupted.(i) in
    let i,j = Map.int_of_pos pos in
    Printf.sprintf "%d,%d" j i
  in

  aux 0

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %s\n"