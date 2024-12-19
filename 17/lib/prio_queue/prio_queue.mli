(** Priority queues *)

module type OrderedType =
  sig
    type t
      (** The type of the queue's priorities. *)

    val compare : t -> t -> int
      (** A total ordering function over the keys.
          This is a two-argument function [f] such that
          [f e1 e2] is zero if the keys [e1] and [e2] are equal,
          [f e1 e2] is strictly negative if [e1] is smaller than [e2],
          and [f e1 e2] is strictly positive if [e1] is greater than [e2].
          Example: a suitable ordering function is the generic structural
          comparison function {!Stdlib.compare}. *)
  end
(** Input signature of the functor {!Make}. *)

module type S =
  sig
    (** This exception is raised when an attempt is made to remove or get an element from an empty queue.*)
    exception Empty

    (** Type for the queue's priorities. *)
    type p

    (** Type of the queue.*)
    type 'a t

    (** Creates a new empty queue. 
    If the boolean is true, 
    the element with the smallest priority will be returned by {!pop}, 
    else the element with largest priority will be returned.*)
    val create: bool -> 'a t

    (** Indicates if the queue is empty.*)
    val is_empty: 'a t -> bool

    (** Pushes an element of the given priority in the queue. *)
    val push: 'a t -> 'a -> p -> unit

    (** Pops out of the queue the element with the biggest or smallest priority, depending on wheather the queue is ascending or descending. 
    If multiple elements have the same smallest or biggest priority, the one picked is not specified.
    @raise Empty if the queue is empty.*)
    val pop: 'a t -> 'a * p

    (** Updates the priority of the first occurence found of the given element (if they are multiple, which one is updated is unspecified). 
        If the element is not in the queue, it gets added.*)
    val update: 'a t -> 'a -> p -> unit
    
    (** Same as {!update}, but nothing happens if the new priority is less urgent as the old one. It also returns true if the priority was changed (or inserted)*)
    val upgrade: 'a t -> 'a -> p -> bool
  end
(** Output signature of the functor {!Make}. *)

module Make (Ord : OrderedType) : S with type p = Ord.t
(** Functor building an implementation of the queue structure
   given a totally ordered type. *)