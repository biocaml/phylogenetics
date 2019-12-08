(** Two-sample multinomial tests *)
type data

val data :
  x1:int array ->
  x2:int array ->
  data

type result = {
  _T_ : float ;
  pvalue : float ;
}

module LRT : sig
  val likelihood_log_ratio : data -> float
  val asymptotic_test : data -> result
  val simulation_test : ?sample_size:int -> data -> result
end

module Sparse : sig
  val likelihood_log_ratio : data -> float
  val asymptotic_test : data -> result
  val simulation_test : ?sample_size:int -> data -> result
end

val uniformity_test :
  k:int ->
  n1:int ->
  n2:int ->
  (data -> result) ->
  unit

val example : data
