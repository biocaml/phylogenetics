(** Simulation for birth-death models

    Provides functionality for simulating tree structures given a
    certain rate for speciation (births) and extinction (deaths)
    rates. *)

type t = private {
  birth_rate : float ;
  death_rate : float ;
}

val make : birth_rate:float -> death_rate:float -> t
(** [make ~birth_rate ~death_rate] creates a birth death parameter object
    with the given birth rate and death rate. It raises an [Invalid_argument]
    exception if either the birth rate or the death rate is negative. *)

val simulation :
  t ->
  Gsl.Rng.t ->
  time:float ->
  (int, int, float) Tree.branch
(** [simulation p rng ~time] simulates a birth death process using the
    parameters [p] and the random number generator [rng] for the given
    [time] duration.  It returns a tree structure where nodes have an
    integer ID and where branches are annotated with their length. *)

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
  Raises [Invalid_argument] if the death rate is greater than the
  birth rate. The algorithm is adapted from the TESS R package, see
  function [tess.sim.taxa.age.constant], and the underlying algorithm
  is described in "Inferring Speciation and Extinction Rates under
  Different Sampling Schemes" by Sebastian Höhna et al. *)
