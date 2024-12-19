module type OrderedType =
  sig
    type t
    val compare : t -> t -> int
  end

module type S =
  sig
    exception Empty
    type p
    type 'a t

    val create: bool -> 'a t
    val is_empty: 'a t -> bool
    val push: 'a t -> 'a -> p -> unit
    val pop: 'a t -> 'a * p
    val update: 'a t -> 'a -> p -> unit
    val upgrade: 'a t -> 'a -> p -> bool
    
  end

module Make (Ord : OrderedType) = struct
  exception Empty
  type p = Ord.t
  type 'a t = (p * 'a * int ref) Dynarray.t * ('a, int ref) Hashtbl.t * (p -> p -> int)
  
  (* auxiliary functions *)
  let update_index queue i =
    let _,_,r = Dynarray.get queue i in
    r := i 
  let swap queue i j = 
    let temp = Dynarray.get queue i in
    Dynarray.set queue i (Dynarray.get queue j);
    Dynarray.set queue j temp;
    update_index queue i;
    update_index queue j
  let left_son i = 2*i + 1
  let right_son i = 2*i + 2
  let parent i = if i = 0 then raise Exit else (i-1)/2
  
  let get_prio queue i = 
    let p,_,_ = Dynarray.get queue i in 
    p
  
  let compare decending p1 p2 = if decending then Ord.compare p2 p1 else Ord.compare p1 p2

  let rec heapify_up cmp queue i = try
    let parent_index = parent i in
    if cmp (get_prio queue i)  (get_prio queue parent_index) > 0 then begin
      swap queue i parent_index;
      heapify_up cmp queue parent_index
    end
  with Exit -> ()

  let rec heapify_down cmp queue i = 
    let p = get_prio queue i in
    
    let left_i = left_son i in
    let right_i = right_son i in 
    let left_p = try Some(left_i |> get_prio queue) with _ -> None in
    let right_p = try Some(right_i |> get_prio queue) with _ -> None in

    let valid p1 = match p1 with 
    | Some(p1) -> cmp p1 p <= 0
    | _ -> true
    in

    if valid left_p && valid right_p then ()
    else 
      let swap i1 = swap queue i1 i ; heapify_down cmp queue i1 in
      match left_p, right_p with
      | Some(_), None -> swap left_i
      | None, Some(_) -> swap right_i
      | Some(p1), Some(p2) when cmp p1 p2 > 0 -> swap left_i
      | _ -> swap right_i

  let remove_tbl tbl e ref = 
    let l = Hashtbl.find_all tbl e in
    while Hashtbl.mem tbl e do
      Hashtbl.remove tbl e
    done;
    List.iter (function ref2 -> if ref2 <> ref then Hashtbl.add tbl e ref2) l
  
  let get_queue (q,_,_) = q

  (* main functions *)
  
  let create descending = Dynarray.create () , Hashtbl.create 42, compare descending

  let is_empty queue = queue |> get_queue |> Dynarray.is_empty
  
  let push queue e p = 
    let queue,tbl,cmp = queue in
    let ref = Dynarray.length queue |> ref in
    Dynarray.add_last queue (p,e,ref);
    Hashtbl.add tbl e ref;
    heapify_up cmp queue (Dynarray.length queue - 1)
  
  let pop queue = let queue,tbl,cmp = queue in try
    let p,e,ref = Dynarray.get queue 0 in
    let last_i = Dynarray.length queue - 1 in
    
    swap queue 0 last_i;
    Dynarray.remove_last queue;
    remove_tbl tbl e ref;
    
    if Dynarray.is_empty queue |> not then heapify_down cmp queue 0;
    
    e,p
  with _ -> raise Empty

  let update_if f queue e p = try
    let queue, tbl, cmp = queue in
    let index = !(Hashtbl.find tbl e) in
    let old_p,e,ref = Dynarray.get queue index in
    Dynarray.set queue index (p,e,ref);
    if f old_p p then begin
      if cmp old_p p < 0 then (heapify_down cmp queue index ; true)
      else if cmp old_p p > 0 then (heapify_up cmp queue index ; true)
      else false
    end else false
    
  with Not_found -> push queue e p ; true

  let update queue e p = update_if (fun _ _ -> true) queue e p |> ignore

  let upgrade queue = 
    let _, _,cmp = queue in
    update_if (fun old_p p -> cmp p old_p > 0) queue

end
