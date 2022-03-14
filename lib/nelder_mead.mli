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
