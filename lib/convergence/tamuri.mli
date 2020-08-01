open Phylogenetics

module Evolution_model : sig
  type param = {
    stationary_distribution : Amino_acid.vector ;
    exchangeability_matrix : Amino_acid.matrix ;
    scale : float ;
  }
  val param_of_wag : Wag.t -> float -> param
  val rate_matrix : param -> Amino_acid.matrix
  val stationary_distribution : param -> Amino_acid.vector
  val transition_probability_matrix : param -> float -> Amino_acid.matrix
end

module Simulator : module type of Simulator.Make(Amino_acid)(Evolution_model)

module Model1 : sig
  val maximum_likelihood :
    exchangeability_matrix:Rate_matrix.Amino_acid.t ->
    stationary_distribution:Amino_acid.vector ->
    (_, Amino_acid.t, float * int) Tree.t ->
    float * float

  val demo : Wag.t -> unit
end

module Model2 : sig
  val demo : ?debug:bool -> Wag.t -> unit
end

module Model3 : sig
  val demo : ?debug:bool -> Wag.t -> unit
end

val lrt_demo : Wag.t -> unit
val lrt_null_demo : ?sample_size:int -> Wag.t -> unit
