type position = int * int
type 'a t = (position, 'a) Hashtbl.t
type direction = int * int

let top = -1,0
let left = 0,-1
let down = 1,0
let right = 0,1

let pos_of_int i j = i,j

let int_of_pos pos = pos

let step (i,j) (di,dj) = i+di,j+dj

let rot_clockwise dir = 
  if      dir = top then right
  else if dir = down then left
  else if dir = left then top
  else                    down

let rot_anticlockwise (di,dj) = rot_clockwise (-di,-dj)

let distance (i,j) (k,l) = abs (i-k) + abs (j-l)

exception Out_of_bounds

let create _ = Hashtbl.create 42

let is_in_bounds = Hashtbl.mem

let get map pos = try Hashtbl.find map pos with _ -> raise Out_of_bounds
let set map = Hashtbl.replace map

let create_rectangle height width f = 
  let map = create () in
  for i = 0 to height-1 do
    for j = 0 to width-1 do
      let pos = pos_of_int i j in
      set map pos (f pos)
    done
  done;
  map

let of_strings f input = 
  let map = create () in
  Array.iteri (fun i str -> 
    String.iteri (fun j c -> 
      try set map (pos_of_int i j) (f c) with Out_of_bounds -> ()) str) input;
  map

let to_strings f e map = 
  let matrix = Dynarray.create () in

  let resize a i default = 
    while Dynarray.length a <= i do
      Dynarray.add_last a (default ())
    done
  in

  let add i j c = 
    resize matrix i Dynarray.create;

    let line = Dynarray.get matrix i in

    resize line j (fun _ -> e);

    Dynarray.set line j c
  in

  Hashtbl.iter (fun pos v -> 
    let i,j = int_of_pos pos in
    add i j (f v)) map;
  
  matrix
  |> Dynarray.map (function a -> a |> Dynarray.to_seq |> String.of_seq)
  |> Dynarray.to_array


let edit map pos f = set map pos (get map pos |> f)

let set_or map pos f g e = 
  try
    let old_e = get map pos in
    set map pos (f e old_e)
  with
  | Out_of_bounds -> set map pos (g e)

let fold f acc map = Hashtbl.fold (fun pos v acc -> f acc pos v) map acc


let find map test = 
  let exception Found of position in
  let f pos v = if test pos v then raise (Found pos) else () in
  try 
    Hashtbl.iter f map;
    raise Not_found
  with Found pos -> pos, get map pos


module Prio = Prio_queue.Make(Int)

let path_find wall directions distance map start finish = 
  let queue = Prio.create true in
  let seen = Hashtbl.create 42 in

  let push (pos, g, path) = Prio.upgrade queue (pos,path) (g + distance pos finish) |> ignore in

  let rec astar () = 
    let (pos,path), f = Prio.pop queue in

    if Hashtbl.mem seen pos then astar () else begin
    
    Hashtbl.add seen pos ();
    
    let g = f - distance pos finish in
    if pos = finish then path
    
    else begin
      (* compute the neighbors *)
      let neighbors = 
        directions
        |> List.map (function dir -> 
          let new_pos = step pos dir in
          new_pos, g + distance pos new_pos, new_pos::path)
      in
      neighbors
      |> List.filter (function pos,_,_ -> try not @@ wall (get map pos) with Out_of_bounds -> false)
      |> List.filter (function pos,_,_ -> Hashtbl.mem seen pos |> not)
      |> List.iter push;
      astar ()
    end end
  
  in 
  push (start,0,[start]);
  astar ()