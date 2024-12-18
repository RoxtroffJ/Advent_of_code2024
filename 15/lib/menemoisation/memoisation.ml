let memoise f = 
  let table = Hashtbl.create 42 in
  fun x -> 
    try Hashtbl.find table x
    with Not_found -> 
      let y = f x in
      Hashtbl.add table x y;
      y

let memoise_rec f =
  let table = Hashtbl.create 42 in
  let rec g x = 
    try Hashtbl.find table x
    with Not_found -> 
      let y = f g x in
      Hashtbl.add table x y;
      y
  in g