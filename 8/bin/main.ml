open Parsing

let tbl_add tbl k e = match Hashtbl.find_opt tbl k with
| None -> Hashtbl.add tbl k [e]
| Some l -> Hashtbl.replace tbl k (e::l)

let parse_input input = 
  let antennas = Hashtbl.create 42 in

  input 
  |> Array.to_seqi 
  |> Seq.map (fun (i,s) -> 
    s 
    |> String.to_seq 
    |> Seq.mapi (fun j c -> ((i,j),c))
  )
  |> Seq.concat
  |> Seq.iter (function pos,c ->
    match c with
    | '.' -> ()
    | _ -> tbl_add antennas c pos
  );

  let height = Array.length input in
  let width = input.(0) |> String.length in
  
  antennas, height, width

let compute_antinodes (i1,j1) (i2,j2) =
  let di = i2 - i1 in
  let dj = j2 - j1 in

  [(i2 + di, j2 + dj); (i1 - di, j1 - dj)],(di,dj)

let list_of_pairs l = 
  let rec aux res = function
  | [] -> res
  | h::t -> 
    let rec build res = function
    | [] -> res
    | b::rest -> build ((h,b)::res) rest
    in
    aux (build res t) t
  in
  aux [] l

let mark_on_map catch map (i,j) = try
  if map.(i).(j) then 0
  else (map.(i).(j) <- true ; Printf.eprintf "m %d,%d " i j; 1)
with
| Invalid_argument s -> if catch then 0 else invalid_arg s

let ex1 input = 
  let antennas, height, width = parse_input input in

  let map = Array.make_matrix height width false in

  let res = Hashtbl.fold (fun freq positions acc -> 
    Printf.eprintf "Freq %c: [ " freq; 
    positions |> List.iter (fun (i,j) -> Printf.eprintf "(%d,%d) " i j); 
    Printf.eprintf "]\n";
    
    let pairs = positions |> list_of_pairs in

    pairs 
    |> List.fold_left (fun acc (pos1,pos2) ->
      Printf.eprintf "\t(%d,%d),(%d,%d) -> " (fst pos1) (snd pos1) (fst pos2) (snd pos2);
      let antinodes = compute_antinodes pos1 pos2 |> fst in
      let add_antinode m = mark_on_map true map m in
      let ret = List.fold_left (fun acc m -> add_antinode m + acc) acc antinodes in
      Printf.eprintf "\n";
      ret
    ) acc
  ) antennas 0
  in

  Array.iter (function line -> 
    Array.iter (function v -> (if v then "#" else ".") |> Printf.eprintf "%s") line ; Printf.eprintf "\n"
  ) map;
  res

let ex2 input = 
  let antennas, height, width = parse_input input in

  let map = Array.make_matrix height width false in

  let res = Hashtbl.fold (fun freq positions acc -> 
    Printf.eprintf "Freq %c: [ " freq; 
    positions |> List.iter (fun (i,j) -> Printf.eprintf "(%d,%d) " i j); 
    Printf.eprintf "]\n";
    
    let pairs = positions |> list_of_pairs in

    pairs 
    |> List.fold_left (fun acc (pos1,pos2) ->
      Printf.eprintf "\t(%d,%d),(%d,%d) -> " (fst pos1) (snd pos1) (fst pos2) (snd pos2);
      
      let antinodes,(di,dj) = compute_antinodes pos1 pos2 in
      
      (* standatd *)

      let add_antinode m = mark_on_map true map m in
      let ret = List.fold_left (fun acc m -> add_antinode m + acc) acc antinodes in
      Printf.eprintf "\n";

      (* aligned *)
      let rec aligned (i,j) (di,dj) res = try
        aligned (i+di,j+dj) (di,dj) (res + mark_on_map false map (i,j))
      with
      | Invalid_argument _ -> res
      in

      ret + aligned pos1 (di, dj) 0 + aligned pos1 (-di, -dj) 0
    ) acc
  ) antennas 0
  in

  Array.iter (function line -> 
    Array.iter (function v -> (if v then "#" else ".") |> Printf.eprintf "%s") line ; Printf.eprintf "\n"
  ) map;
  res

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"