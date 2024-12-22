(** Library to manage adjacency graphs, with all edges having a wheight of 1.*)

(** Type of a graph with nodes identified with an object of type ['a].*)
type 'a t

(** Creates an empty graph (no nodes nor edges).*)
val create: unit -> 'a t

(** [add graph node1 node2] adds a directionnal vertice from [node1] to [node2] in [graph]. 
If one or both nodes did not exist, then they are added.
If the edge existed already, then nothing happens.*)
val add: 'a t -> 'a -> 'a -> unit

(** Same as add, but the edge is bidirecionnal.*)
val add_bidir: 'a t -> 'a -> 'a -> unit

(** Adds vertices (and nodes if required) using a function [f]. For each nodes, a vertice is added to the nodes returned by [f]*)
val add_with_fun: 'a t -> ('a -> 'a list) -> unit 

(** Returns the neighbors of the given node.*)
val get_neighbors: 'a t -> 'a -> 'a Seq.t

(** [astar h graph start finish] returns the shortest path from [start] to [finish].using the A* algorithm with the heuristic [h]*)
val astar: (string -> string -> int) -> string t -> string -> string -> string list