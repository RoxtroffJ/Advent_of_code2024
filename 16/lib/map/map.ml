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

let of_strings f input = 
  let map = create () in
  Array.iteri (fun i str -> 
    String.iteri (fun j c -> 
      try set map (pos_of_int i j) (f c) with Out_of_bounds -> ()) str) input;
  map

let edit map pos f = set map pos (get map pos |> f)

let set_or map pos f g e = 
  try
    let old_e = get map pos in
    set map pos (f e old_e)
  with
  | Out_of_bounds -> set map pos (g e)

let find map test = 
  let exception Found of position in
  let f pos v = if test pos v then raise (Found pos) else () in
  try 
    Hashtbl.iter f map;
    raise Not_found
  with Found pos -> pos, get map pos