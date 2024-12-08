open Parsing
type square = Empty | Obstacle

let parse_map input = 
  let start = ref (-1,-1) in
  let map = Array.mapi 
    (fun i s -> Printf.eprintf "\n";
      s 
      |> String.to_seq 
      |> Seq.mapi 
        (fun j c -> Printf.eprintf "%c" c; match c with
          | '^' -> start := i,j ; Printf.eprintf "Start %d %d" (!start |> fst) (!start |> snd) ; Empty
          | '.' -> Empty 
          | '#' -> Obstacle 
          | _ -> raise (Invalid_argument "parse_map")
        )
      |> Array.of_seq
    )
    input
  in
  map, !start

let _ =
  let input = "input" |> load in
  
  let ex1 = 
    let map,start = parse_map input in

    let seen = Array.make_matrix (Array.length map) (Array.length map.(0)) false in

    let turn_right (di,dj) = 
      if di = 1 then (0,-1)
      else if dj = -1 then (-1,0)
      else if di = -1 then (0,1)
      else (1,0)
    in

    let step (i,j) (di,dj) = 
      Printf.eprintf "%d,%d going %d,%d\n" i j di dj;
      let new_i,new_j = i+di,j+dj in
      match map.(i+di).(j+dj) with
      | Empty -> (if seen.(new_i).(new_j) then 0 else (seen.(new_i).(new_j) <- true ; 1)), (new_i, new_j), (di, dj)
      | Obstacle -> 0,(i,j),turn_right (di,dj)
    in

    let rec walk pos dir res = try
      let a, n_pos, n_dir = step pos dir in
      walk n_pos n_dir (res + a)
    with _ -> res
    in

    walk start (-1,0) 0
  in

  let ex2 = 

    (* INITIALISATION *)

    let map,start = parse_map input in
    let height = Array.length map in
    let width = Array.length map.(0) in

    let seen = Array.make_matrix (height) (width) [] in

    let turn_right (di,dj) = 
      if di = 1 then (0,-1)
      else if dj = -1 then (-1,0)
      else if di = -1 then (0,1)
      else (1,0)
    in

    let step (i,j) (di,dj) f = 

      f (i,j) (di,dj);

      let new_i,new_j = i+di,j+dj in
      match map.(i+di).(j+dj) with
      | Empty -> (new_i, new_j), (di, dj)
      | Obstacle -> (i,j),turn_right (di,dj)
    in

    let rec walk pos dir f = try
      let n_pos, n_dir = step pos dir f in
      walk n_pos n_dir f
    with Invalid_argument _ -> ()
    in

    (* STANDARD PATH *)

    let mark seen (i,j) dir = 
      let trace = seen.(i).(j) in
      seen.(i).(j) <- dir::trace
    in

    walk start (-1,0) (mark seen);
    
    seen 
    |> Array.mapi 
      (fun i a -> a |> Array.mapi (fun j b -> (b, (i,j))))
    |> Array.fold_left
      (Array.fold_left 
        (fun acc (_,pos) -> 
          let i,j = pos in
          acc + match seen.(i).(j) with
          | [] -> 0
          | _ when pos = start -> 0
          | _ -> begin
            map.(i).(j) <- Obstacle;
            Printf.eprintf "Killed %d,%d\n" i j;
            let exception Cycle in
            let cycle seen (i,j) dir = if seen.(i).(j) |> List.exists ((=) dir) then raise Cycle in

            let seen = Array.make_matrix height width [] in

            (* Printf.eprintf "\n\n%d %d\n\n" i j; *)

            let f pos dir = 
              (* let (i,j),(di,dj) = pos,dir in
              Printf.eprintf "%d,%d\tgoing %d,%d\n" i j di dj; *)
              cycle seen pos dir;
              mark seen pos dir
            in

            let c = try walk start (-1,0) f ; 0 with Cycle -> Printf.eprintf "Found\n" ; 1 in
            map.(i).(j) <- Empty;
            c
          end
        )
      )
      0
  in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2