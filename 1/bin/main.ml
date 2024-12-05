open Parsing

let _ =
  let input = "input" |> load in
  
  let ex1 = 
    let left,right = 
      input
      |> Array.map (function s -> Scanf.sscanf s "%d%[ ]%d" (fun a _ b -> (a,b)))
      |> Array.split
    in
    Array.sort compare left;
    Array.sort compare right;
    
    Array.combine left right
    |> Array.map (function (a,b) -> abs(a-b))
    |> Array.fold_left (+) 0 
  in

  let ex2 = 
    let left,right = 
      input
      |> Array.map (function s -> Scanf.sscanf s "%d%[ ]%d" (fun a _ b -> (a,b)))
      |> Array.split
    in
    
    let occ =
      let tbl = Hashtbl.create 42 in
      let add a = 
        try 
          let c = Hashtbl.find tbl a in
          Hashtbl.replace tbl a (c+1)
        with
        | Not_found -> Hashtbl.add tbl a 1
      in
      Array.iter add right;
      function x -> try Hashtbl.find tbl x with Not_found -> 0
    in

    left
    |> Array.to_seq
    |> Seq.map (fun a -> let nb_a = occ a in Printf.eprintf "%d appears %d times\n" a nb_a ; a * nb_a)
    |> Seq.fold_left (+) 0 
  in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2