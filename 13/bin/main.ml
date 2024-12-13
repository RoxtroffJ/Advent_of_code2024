open Parsing

let a_cost = 3
let b_cost = 1
let max_but_press = 100

(* Mashine is the tuple Xa,Ya,Xb,Yb,X,Y *)
let parse_machine s = try
  Scanf.sscanf s "Button A: X%d , Y%d\nButton B: X%d , Y%d\nPrize: X=%d , Y=%d%n" (fun xa ya xb yb x y i -> (xa,ya,xb,yb,x,y), String.sub s i (String.length s - i))
with _ -> raise Parsing_Failure

let string_to_mashines s = s 
  |> split_list (parse_word "\n\n")
  |> List.map (fun (s,_) -> parse_machine s |> fst)

let load filename = 
  let array = load filename in 

  Array.fold_left (fun acc str -> acc ^ str ^ "\n") "" array |> string_to_mashines

let cost = function 
   | None -> 0
   | Some(a,b) -> a_cost*a + b_cost*b

let find_best a1 b1 a2 b2 = 
  let c1 = match a1,b1 with
  | None,None -> None
  | Some(a),Some(b) -> Some(a,b)
  | _ -> invalid_arg "find best"
  in

  let c2 = match a2,b2 with
  | None,None -> None
  | Some(a),Some(b) -> Some(a,b)
  | _ -> invalid_arg "find best"
  in

  match c1,c2 with
  | None,_ -> c2
  | _,None -> c1
  | Some(a1,b1), Some(a2,b2) -> if Some(a1,b1) |> cost < (Some(a2,b2) |> cost) then Some(a1,b1) else Some(a2,b2)


(* Finds the cheapest solution for 
  xa * a + xb * b = x
  ya * a + yb * b = y

  |xa xb||a|   |x|
  |ya yb||b| = |y|

  XA = Y

  |a|        | yb -xb||x|
  |b| = 1/det|-ya  xa||y| 
*)

let solve_machine_opt (xa,ya,xb,yb,x,y) = 
  (* Printf.eprintf "\tSolving...\n"; *)
  
  let det = xa*yb - xb*ya in
  if det = 0 then 
    Printf.eprintf "\tdet = %d\n\n" det;

  if det <> 0 then (* Found out afterwards that this is always true with my input... *)
    let a = (yb*x - xb*y)/det in
    let b = (-ya*x + xa*y)/det in

    if a >= 0 && b >= 0 && a*xa + b*xb = x && a*ya + b*yb = y then Some(a,b) else None
  else 
    let min_b = 
      let rec find b = if x-b*xb mod xa = 0 then Some(b) else if b < max_but_press then find (b+1) else None in
      find 0
    in
    let min_a = 
      let rec find a = if x-a*xa mod xb = 0 then Some(a) else if a < max_but_press then find (a+1) else None in
      find 0
    in
    let max_a = match min_b with
    | None -> None 
    | Some b -> Some(b/xa) 
    in
    let max_b = match min_a with
    | None -> None 
    | Some a -> Some(a/xb) 
    in

    find_best min_a max_b max_a min_b

let _machine_to_string (x1,y1,x2,y2,x,y) = Printf.sprintf "Button A: X+%d, Y+%d\nButton B: X+%d, Y+%d\nPrize: X=%d, Y=%d" x1 y1 x2 y2 x y

let ex1 input = input
    |> List.fold_left (fun acc m -> (*Printf.eprintf "%s\n" (machine_to_string m) ;*) m
      |> solve_machine_opt
      |> cost
      |> (+) acc) 0

let pos_offset = 10000000000000
let update_machine (x1,y1,x2,y2,x,y) = (x1,y1,x2,y2,x+pos_offset,y+pos_offset)

let ex2 input = 
  input
  |> List.map update_machine
  |> ex1

let _ =
  let input = "input" |> load in

  input |> ex1 |> Printf.printf "Result 1: %d\n";
  input |> ex2 |> Printf.printf "Result 2: %d\n"