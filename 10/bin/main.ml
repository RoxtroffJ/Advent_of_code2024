open Parsing

let int_from_char c = int_of_char c - int_of_char '0'

let parse_input input = 
  let seq_map = input |> Array.to_seq |> Seq.map (fun line -> line |> String.to_seq |> Seq.map int_from_char) in
  let start_pos = 
    seq_map
    |> Seq.fold_lefti (fun acc i -> 
      Seq.fold_lefti (fun acc j c -> 
        if c = 0 then (i,j)::acc else acc
      ) acc
    ) []
  in

  seq_map |> Seq.map Array.of_seq |> Array.of_seq, start_pos

let directions = [(0,1);(0,-1);(1,0);(-1,0)]

let next_pos (i,j) (di,dj) = (i+di,j+dj)

let trail_score1 map start = 
  let seen = Hashtbl.create 42 in

  let rec aux res (i,j) = try
    if map.(i).(j) = 9 then 
      try 
        let _ = Hashtbl.find seen (i,j) in
        res
      with
      | Not_found -> Hashtbl.add seen (i,j) () ; res + 1
    else
      directions 
      |> List.map (next_pos (i,j)) 
      |> List.filter (fun (i1,j1) -> try map.(i1).(j1) = map.(i).(j) + 1 with Invalid_argument _ -> false)
      |> List.fold_left aux res
  with Invalid_argument _ -> res
  in

  aux 0 start

let trail_score2 map start = 
  let rec aux res (i,j) = try
    if map.(i).(j) = 9 then res (* branch is valid *)
    else
      let next = 
        directions 
        |> List.map (next_pos (i,j)) 
        |> List.filter (fun (i1,j1) -> try map.(i1).(j1) = map.(i).(j) + 1 with Invalid_argument _ -> false)
      in match next with
      | [] -> res - 1 (* invalid *)
      | _ -> List.fold_left aux (res + List.length next - 1) next
  with Invalid_argument _ -> res - 1 (* branch is invalid *)
  in

  aux 1 start
    
let ex trail_score input = 
  let map, start = input |> parse_input in
  start |> List.fold_left (fun acc pos -> acc + trail_score map pos) 0 

let ex1 = ex trail_score1
let ex2 = ex trail_score2

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"