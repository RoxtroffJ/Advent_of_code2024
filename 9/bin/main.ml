open Parsing

let line_to_num line = 
  let rec aux res = function
  | "" -> res
  | line -> aux (int_of_char line.[0] - (int_of_char '0') :: res) (String.sub line 1 (String.length line - 1))
  in
  aux [] line |> List.rev 

let num_to_mem num = 
  let res = Dynarray.create () in
  let rec aux i num = match num with
  | [] -> ()
  | n::t -> Dynarray.add_last res (n, if i mod 2 = 0 then Some(i/2) else None); aux (i + 1) t
  in
  aux 0 num;
  res

let rec add_nth array i elt = try
  let new_elt = Dynarray.get array i in
  Dynarray.set array i elt;
  add_nth array (i + 1) new_elt
with Invalid_argument _ -> Dynarray.add_last array elt

let check_sum mem = 
  let rec sum res i l mem = try match Dynarray.get mem i with
  | 0, _ -> sum res (i + 1) l mem
  | len, id -> Dynarray.set mem i (len-1, id) ; sum (res + (match id with None -> 0 | Some(id) -> id) * l) i (l+1) mem
  with Invalid_argument _ -> res
  in

  sum 0 0 0 mem

let ex1 input = 
  let mem = input.(0) |> line_to_num |> num_to_mem in
  
  let rec compact i = 
    if i = Dynarray.length mem then ()
    else if Dynarray.get mem i|> snd |> Option.is_some then compact (i + 1)
    else begin
      let len,_ = Dynarray.get mem i in
      match Dynarray.get_last mem with
      | _,None -> Dynarray.remove_last mem; compact i
      | last_len,Some(id) when last_len <= len -> 
        Dynarray.remove_last mem;
        Dynarray.set mem i (last_len,Some(id));
        add_nth mem (i + 1) (len - last_len, None);
        compact (i+1)
      | last_len,Some(id) ->
        Dynarray.set mem i (len,Some(id));
        let length = Dynarray.length mem in
        Dynarray.set mem (length - 1) (last_len - len, Some(id));
        compact (i+1)
    end
  in
  compact 0;

  check_sum mem

let print_mem out map = 
  let rec aux i = try 
    begin 
      match Dynarray.get map i with
      | 0, _ -> ()
      | len, None -> for _ = 0 to len - 1 do Printf.fprintf out "." done
      | len, Some(id) -> for _ = 0 to len - 1 do Printf.fprintf out "%d" id done
    end; aux (i + 1)
  with Invalid_argument _ -> ()
  in
  aux 0 ; Printf.fprintf out "\n"

let ex2 input = 
  let mem = input.(0) |> line_to_num |> num_to_mem in
  
  (* j in decreasing order *)
  let rec compact j = (* Printf.eprintf "%d -> " j ; print_mem stderr mem ;*) try match Dynarray.get mem j with
  | 0, _
  | _, None -> compact (j - 1)
  | len, Some(id) -> 
    Dynarray.set mem j (len, None);
    let rec place i = match Dynarray.get mem i with
    | e_len, None when e_len >= len -> 
      Dynarray.set mem i (len, Some(id));
      add_nth mem (i+1) (e_len - len, None);
      i
    | _ -> place (i + 1)
    in
    let i = place 0 in
    compact (if i = j then j - 1 else j)
  with Invalid_argument _ -> (*Printf.eprintf "Stopped at j=%d\n" j ;*) ()
  in
  compact (Dynarray.length mem - 1);
  print_mem stderr mem;
  check_sum mem

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"