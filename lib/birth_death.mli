(** Simulation for birth-death models *)

type t = private {
  birth_rate : float ;
  death_rate : float ;
}

val make : birth_rate:float -> death_rate:float -> t

val simulation :
  t ->
  Gsl.Rng.t ->
  time:float ->
  (int, int, float) Tree.branch

val age_ntaxa_simulation :
  ?sampling_probability:float ->
  t ->
  Gsl.Rng.t ->
  age:float ->
  ntaxa:int ->
  (unit, int, float) Tree.t
(** [age_ntaxa_simulation p rng ~age ~ntaxa] simulates a birth death
  process of parameters [p] using random generator [rng],
  conditioned on the age of the MRCA being [age] and having [ntaxa]
  leaves.
  @raises [Invalid_arg] if the death rate is greater than the
  birth rate. The algorithm is adapted from the TESS R package, see
  function [tess.sim.taxa.age.constant], and the underlying algorithm
  is described in "Inferring Speciation and Extinction Rates under
  Different Sampling Schemes" by Sebastian Höhna et al. *)
