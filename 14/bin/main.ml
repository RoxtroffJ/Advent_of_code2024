open Parsing

type robot = {
  position: int*int;  (* i,j convention *)
  speed: int*int      (* i,j convention *)
}

type map = {
  width: int;
  height: int;
  robots: robot array
}

let string_to_robot s = 
  let i,j,di,dj = Scanf.sscanf s "p=%d,%d v=%d,%d" (fun x y dx dy -> y,x,dy,dx) in
  {
    position = i,j;
    speed = di,dj
  }

let input_to_map input = 
  let width,height = Scanf.sscanf input.(0) "%d,%d" (fun x y -> x,y) in
  let input = Array.sub input 1 (Array.length input - 1) in

  {
    width;
    height;
    robots = Array.map string_to_robot input
  }

let map_to_string map = 
  let a = Array.make_matrix map.height map.width 0 in
  map.robots |> Array.iter (fun robot -> let i,j = robot.position in a.(i).(j) <- a.(i).(j) + 1);
  Array.fold_left (fun acc line -> acc ^
    Array.fold_left (fun acc n -> acc ^ match n with
    | 0 -> "."
    | _ -> n |> string_of_int) "" line ^ "\n") "" a

let robot_step height width robot = 
  {robot with position = 
    let (i,j) = robot.position in
    let (di,dj) = robot.speed in

    let ni,nj = i+di, j+dj in

    (if ni >= 0 then ni mod height else ni + height), (if nj >= 0 then nj mod width else nj + width)
  }

let map_step map = 
  {map with robots = map.robots |> Array.map (robot_step map.height map.width)}

let rec map_many_steps i map =
  if i <= 0 then map else map_many_steps (i-1) (map_step map)

let get_quadrant_robots map = 
  let robots = map.robots in
  let split_height = map.height/2 in
  let split_width = map.width/2 in

  Array.fold_left (fun arr robot -> match arr with [|first;second;third;fourth|] ->
    let i,j = robot.position in

    if      i < split_height && j < split_width then [|robot::first;second;third;fourth|]
    else if i < split_height && j > split_width then [|first;robot::second;third;fourth|]
    else if i > split_height && j < split_width then [|first;second;robot::third;fourth|]
    else if i > split_height && j > split_width then [|first;second;third;robot::fourth|]
    else [|first;second;third;fourth|]
    |_ -> failwith "get_quadrant_robot"
  ) [|[];[];[];[]|] robots

let ex1 input = 
  input
  |> input_to_map
  |> map_many_steps 100
  |> get_quadrant_robots
  |> Array.map List.length
  |> Array.fold_left ( * ) 1
  

let ex2 input = 
  let map = input_to_map input in

  let output = open_out "output" in

  let rec aux i map = 
    let str = map_to_string map in
    Printf.fprintf output "Step %d\n%s\n" i str;
    
    if String.exists (function c -> not (c = '.' || c = '1' || c = '\n')) str |> not then begin
      Printf.printf "Step %d\n%s\n" i str;

      match read_line() with
      | "" -> aux (i+1) (map_step map)
      | _ -> i
    end else aux (i+1) (map_step map)
  in 
  let res = aux 0 map in
  close_out output;
  res

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"