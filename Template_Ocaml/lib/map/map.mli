(** This library is used to manage 2D mutable maps. *)

(** {1:types Types}*)

(** Type for a map *)
type 'a t

(** Type for a position in the map *)
type position

(** Type for a direction in the map *)
type direction

(** {1:position_manipulation Position manipulation}*)

(** The top (or north) direction*)
val top: direction

(** The down (or south) direction*)
val down: direction

(** The left (or west) direction*)
val left: direction

(** The right (or east) direction*)
val right: direction

(** Converts line and row indexes into a position *)
val pos_of_int: int -> int -> position

(** Converts a pos into line and row indexes *)
val int_of_pos: position -> int * int

(** Returns the position after one step in the given direction *)
val step: position -> direction -> position

(** Rotates the direction 90° clockwise*)
val rot_clockwise: direction -> direction

(** Same as {!rot_clockwise}, but anticlockwise*)
val rot_anticlockwise: direction -> direction

(** Computes the Manattan distance between the two positions *)
val distance: position -> position -> int

(** {1:map_manipulation Map manipulation}*)

(** This exception indicates that a position is out of the map*)
exception Out_of_bounds

(** Creates a new empty map*)
val create: unit -> 'a t

(** Converts an array of strings into a map. 
    The returned map will be arranged the same way as the output of [Array.iter print_endline] called on the array of strings
    To convert a character into a map element, the provided function is used. If it raises the {!Out_of_bounds} exception, then the character is ignored.*)
val of_strings: (char -> 'a) -> string array -> 'a t

(** Reverse of {!of_strings}. The extra provided character will be used to offset lines if needed.*)
val to_strings: ('a -> char) -> char -> 'a t -> string array

(** [creates height width f] creates a rectangle map, filled with the returned values of [f]*)
val create_rectangle: int -> int -> (position -> 'a) -> 'a t 

(** Indicates if the given position is in the bounds of the map.*)
val is_in_bounds: 'a t -> position -> bool

(** Gets the value on the provided position in the given map. 
@raise Out_of_bounds if the position is out of the map.*)
val get: 'a t -> position -> 'a

(** Sets the given value at the provided position on the map.*)
val set: 'a t -> position -> 'a -> unit

(** Runs the provided function on the element at the given position. Replaces that element by the result.
@raise Out_of_bounds if the position is out of the map.*)
val edit: 'a t -> position -> ('a -> 'a) -> unit

(** If the map does not contain anything at the given position, [set_or map pos f g e] is [set map pos (g e)]. Else it is [edit map pos (f e)]*)
val set_or: 'a t -> position -> ('b -> 'a -> 'a) -> ('b -> 'a) -> 'b -> unit

(** Folds on all the cells, in an arbitrary order. The behavior is unspecified if the provided function modifies the map.*)
val fold: ('b -> position -> 'a -> 'b) -> 'b -> 'a t -> 'b

(** Finds the first occurence that matches the provided test, and returns it. Raises [Not_found] if there is no such element in the map.*)
val find: 'a t -> (position -> 'a -> bool) -> (position * 'a)

(** [path_find wall directions distance map start finish] finds and returns the shortest path (according to the [distance] function from [start] to [finish]. 
The path is continus, does not cross cells for which the [wall] function returns [true], 
and the direction between two consequtive cells is always in the [direction] list.*)
val path_find: ('a -> bool) -> direction list -> (position -> position -> int) -> 'a t -> position -> position -> position list