open Parsing

let directions = [(1, 0);(0, 1);(-1, 0);(0, -1)]


let ex1 input = 
  let seen = Hashtbl.create 42 in

  let valid c (i,j) = try Hashtbl.find_opt seen (i,j) |> Option.is_none, input.(i).[j] = c with _ -> true, false in

  let compute_region c (i,j) =
    (* Computes the perimeter and area of the region *)
    
    let rec aux perimeter area (i,j) = 
      Hashtbl.add seen (i,j) ();
      (* Printf.eprintf "\tWent to %d,%d\n" i j ; *)
      let next_full = List.map (fun (di, dj) -> (i+di, j+dj)) directions in
      let next = next_full |> List.filter (fun pos -> let a,b = pos |> valid c in a && b) in
      let nb_nei = next_full |> List.fold_left (fun acc pos -> acc + let _,a = valid c pos in if a then 1 else 0) 0 in
      let ret = perimeter + 4 - nb_nei, area + 1 in
      match next with
      | [] -> ret
      | _ -> 
        next
        |> List.fold_left (fun (p, a) (i, j) -> 
          if valid c (i,j) |> fst then aux p a (i, j) else p,a
        ) ret
    in
    aux 0 0 (i, j)
  in

  input
    |> Array.to_seq
    |> Seq.fold_lefti (fun acc i line -> 
      line |> 
      String.to_seq
      |> Seq.fold_lefti (fun acc j c -> 
        if Hashtbl.find_opt seen (i,j) |> Option.is_none then begin
          let p,a = compute_region c (i,j) in
          Printf.eprintf "Region %c (%d,%d) -> area %d, perimeter %d\n" c i j a p;
          acc + p*a
        end else acc
      ) acc
    ) 0

let ex2 input = 
  let seen = Hashtbl.create 42 in

  let compute_region c (i,j) =
    (* Computes the area and the number of corners of the region *)
    
    let neighbors = Hashtbl.create 42 in
    let inside = Hashtbl.create 42 in
    let seen_corners = Hashtbl.create 42 in

    let add_corner overwrite (i,j) (di1,dj1) (di2,dj2) = try
      let di,dj = match di1 + di2, dj1 + dj2 with
      | -1,-1 ->  0, 0
      | -1, 1 ->  0, 1
      |  1,-1 ->  1, 0
      |  1, 1 ->  1, 1
      | _ -> raise Exit
      in

      let ci,cj = i+di, j+dj in
      
      Printf.eprintf "\tadding corner %d,%d\n" ci cj;

      (if overwrite then Hashtbl.replace else Hashtbl.add) seen_corners (ci,cj) ()
    with Exit -> ()
    in

    let valid nei c (i,j) = try 
      let a = Hashtbl.find_opt inside (i,j) |> Option.is_none in 
      let b = input.(i).[j] = c in

      (match nei with
      | None -> ()
      | Some nei -> if not b then Hashtbl.replace nei (i,j) ());

      a,b
    with _ -> true, false in


    let rec aux area (i,j) = 
      let add_corner = add_corner false (i,j) in
      Hashtbl.replace seen (i,j) ();
      Hashtbl.replace inside (i,j) ();
      (* Printf.eprintf "\tWent to %d,%d\n" i j ; *)
      let next_full = List.map (fun (di, dj) -> (i+di, j+dj)) directions in
      let next = next_full |> List.filter (fun pos -> let a,b = pos |> valid (Some neighbors) c in a && b) in
     
      let l = next_full |> List.filter (fun pos -> pos |> valid (Some neighbors) c |> snd |> not) |> List.map (fun (i1,j1) -> (i1 - i, j1 -j)) in
      begin match l with
      | [] 
      | [_] -> ()
      | [a;b;c] -> add_corner a b ; add_corner b c ; add_corner a c
      | [a;b;c;d] -> add_corner a b ; add_corner b c ; add_corner c d ; add_corner d a
      | [a;b] -> add_corner a b
      | _ -> invalid_arg "corners 1"
      end;
      
      let ret = area + 1 in
      match next with
      | [] -> ret
      | _ -> 
        next
        |> List.fold_left (fun a (i, j) -> 
          if valid (Some neighbors) c (i,j) |> fst then aux a (i, j) else a
        ) ret
    in
    let area = aux 0 (i, j) in

     
    Hashtbl.iter (fun (i,j) _ ->
      let add_corner = add_corner true (i,j) in
      let next_full = List.map (fun (di, dj) -> (i+di, j+dj)) directions in
      let l = next_full |> List.filter (fun pos -> pos |> valid None c |> fst |> not) |> List.map (fun (i1,j1) -> (i1 - i, j1 -j)) in
      match l with
        | [] 
        | [_] -> ()
        | [a;b;c] -> add_corner a b ; add_corner b c ; add_corner a c
        | [a;b;c;d] -> add_corner a b ; add_corner b c ; add_corner c d ; add_corner d a
        | [a;b] -> add_corner a b
        | _ -> invalid_arg "corners 2") neighbors;
    
    Hashtbl.length seen_corners, area
  in

  input
    |> Array.to_seq
    |> Seq.fold_lefti (fun acc i line -> 
      line |> 
      String.to_seq
      |> Seq.fold_lefti (fun acc j c -> 
        if Hashtbl.find_opt seen (i,j) |> Option.is_none then begin
          let p,a = compute_region c (i,j) in
          Printf.eprintf "Region %c (%d,%d) -> area %d, sides %d\n" c i j a p;
          acc + p*a
        end else acc
      ) acc
    ) 0

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"