(** Module to help with parsing text. There are functions to load a file, and parsers.

The parsers either suceed or raise an exception. When they succeed, they return a result and the rest of the text *)

(** {1:general General}*)

(** The exception raised by all the parsers *)
exception Parsing_Failure

(** The type of a parser *)
type 'a parser = string -> ('a * string)

(** Loads a text file and returns the array of lines *)
val load : string -> string array

(** {1:parsers Parsers}*)

(** Checks if the string starts by the [word] *)
val parse_word : string -> unit parser

(** Same as parse_word but returns the provided ['a] instead of an empty unit *)
val parse_word_res : 'a -> string -> 'a parser

(** Reads an integer (see [Scanf] with %d) *)
val parse_int : int parser

(** {1:usage Usage} *)

(** Chains two parsers *)
val combine_parsers : 'a parser -> 'b parser -> ('a * 'b) parser

(** Same as {! Parsing.combine_parsers} *)
val (+>) : 'a parser -> 'b parser -> ('a * 'b) parser

(** Tries to parse with first parser. If fails, tries to parse with second parser *)
val try_or : 'a parser -> 'a parser -> 'a parser

(** Same as {!try_or} *)
val (<|>) : 'a parser -> 'a parser -> 'a parser

(** Finds all the matching occurences in the text, without checking for overlaping matches *)
val find_all : 'a parser -> string -> 'a list

(** Splits the text as a list separated by something matching the provided parser. All the elements are also associated to the returned value of the previous separator (except first one) *)
val split_list : 'a parser -> string -> (string * 'a option) list
