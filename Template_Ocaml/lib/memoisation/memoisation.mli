(** This module helps you memoise your functions *)

(** Memoises a non recursive function. *)
val memoise : ('a -> 'b) -> ('a -> 'b)

(** Memoises a recursive function. 
    
Unlike {!memoise}, when calling [memoise_rec f], the parameter [f] is not directly the function to memoise. 
[f] takes two parameters : an other function [g] and the arguments of the function to be memoised.
[f] is then coded as the standard recursive function, but it calls [g] instead of calling itself.
For example, here is how to do the fibbonacci function:
    
[let fibbo = memoise_rec (fun f n -> if n < 2 then n else f (n-1) + f (n-2))] *)
val memoise_rec : (('a -> 'b) -> 'a -> 'b) -> ('a -> 'b)