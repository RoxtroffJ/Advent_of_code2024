type 'a t = {
  mutable neighbors: 'a -> 'a Seq.t
}

let create _ = {neighbors = function _ -> Seq.empty}

let cons_seq_no_double x seq =  
  let rec aux s () = match s () with
  | Seq.Nil -> Seq.Cons (x, Seq.empty)
  | Seq.Cons(a, next) when a = x -> Seq.Cons(x, next)
  | Seq.Cons(a, next) -> Seq.Cons(a, aux next)
  in
  aux seq 

let add graph node1 node2 = 
  let old_f = graph.neighbors in
  let new_f = function 
  | node when node = node1 -> cons_seq_no_double node2 (old_f node)
  | node -> old_f node 
  in

  graph.neighbors <- new_f

let add_bidir graph node1 node2 = 
  add graph node1 node2;
  add graph node2 node1

let merge_seq_no_doubles seq1 =
  let memo = 
    seq1
    |> Seq.map (fun x -> x,())
    |> Hashtbl.of_seq
  in

  function seq2 -> 
    let rec update s2 () = match s2() with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons(x,next) when Hashtbl.mem memo x -> update next ()
    | Seq.Cons(x,next) -> Cons(x, update next)
    in

    let rec merge s1 s2 () = match s1 () with
    | Seq.Nil -> update s2 ()
    | Seq.Cons(x,next) -> Seq.Cons(x, merge next s2)
    in

    merge seq1 seq2

let add_with_fun graph f = 
  let old_f = graph.neighbors in
  let new_f = function node -> merge_seq_no_doubles (old_f node) (f node |> List.to_seq) in

  graph.neighbors <- new_f

let get_neighbors graph node = graph.neighbors node

module Prio = Prio_queue.Make(Int)

let astar h graph start finish = 
  let queue = Prio.create true in
  let seen = Hashtbl.create 42 in

  let push (pos, g, path) = Prio.upgrade queue (pos,path,g) (g + h pos finish) |> ignore in

  let rec aux () = 
    let (pos,path,g), _f = Prio.pop queue in

    if Hashtbl.mem seen pos then aux () else begin
    Hashtbl.add seen pos ();
    
    if pos = finish then path
    
    else begin
      (* compute the neighbors *)
      let neighbors = 
        get_neighbors graph pos
        |> Seq.map (function n_pos -> n_pos, g+1, n_pos::path)
      in
      neighbors
      |> Seq.filter (function pos,_,_ -> Hashtbl.mem seen pos |> not)
      |> Seq.iter push;
      aux ()
    end end
  
  in 
  push (start,0,[start]);
  aux () |> List.rev
  