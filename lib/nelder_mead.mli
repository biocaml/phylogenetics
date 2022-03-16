(** An implementation of Nelder-Mead algorithm for function
   optimization

   Implements method as described in
   {{:https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method}Wikipedia}
   and uses some tests from the original publication:

     A simplex method for function minimization
     J. A. Nelder and R. Mead
 *)

(** Minimizes cost function [f].
    [sample] function must return a vector of initial parameters.
    Returns the minimum value of [f], the vector of optimized parameters,
    and the number of iterations.
*)
val minimize :
  ?tol:float ->
  ?maxit:int ->
  ?debug:bool ->
  f:(float array -> float) ->
  sample:(unit -> float array) ->
  unit ->
  float * float array * int
