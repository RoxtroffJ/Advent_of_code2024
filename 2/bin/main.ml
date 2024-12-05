open Parsing

let _ =
  let input = "input" |> load in
  
  let min_spacing = 1 in
  let max_spacing = 3 in

  let ex1 = input |> Array.fold_left
    (fun acc line -> 
      
      let nb_list = 
        String.split_on_char ' ' line
        |> List.map int_of_string
      in

      let rec check cmp prev l = match l with
        | [] -> true
        | h::t -> cmp prev h && abs (h - prev) >= min_spacing && abs (h - prev) <= max_spacing && check cmp h t
      in

      let h,t = List.hd nb_list, List.tl nb_list in

      acc + if check (<=) h t || check (>=) h t then 1 else 0
    )
    0
  in
  
  let ex2 = input |> Array.fold_left
    (fun acc line -> 

      let nb_list = 
        String.split_on_char ' ' line
        |> List.map int_of_string
      in

      let valid cmp ao b = match ao with
      | None -> true
      | Some a -> cmp a b && abs (a - b) <= max_spacing && abs (a - b) >= min_spacing 
      in
      
      let rec check jocker cmp prev l = match l with
      | a::b::t when valid cmp (Some a) b -> check jocker cmp (Some a) (b::t)
      | a::b::t when not jocker -> check true cmp None (a::t) || 
        begin match prev with 
        | None -> check true cmp None (b::t)
        | Some p -> check true cmp None (p::b::t)
        end
      | a::b::t -> false
      | _ -> true
        
      in

      acc + if check false (<=) None nb_list || check false (>=) None nb_list then 1 else 0
      )
    0 
  in

  Printf.printf "Result 1: %d\n" ex1;
  Printf.printf "Result 2: %d\n" ex2