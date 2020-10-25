module Convsim = Simulator
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

type simulation = (Amino_acid.t, Amino_acid.t, float * int) Tree.t

type likelihood_ratio_test = {
  full_log_likelihood : float ;
  reduced_log_likelihood : float ;
  _D_ : float ;
  df : float ;
  pvalue : float ;
}

module Model1 : sig
  type param = float

  val maximum_log_likelihood :
    ?debug:bool ->
    exchangeability_matrix:Rate_matrix.Amino_acid.t ->
    stationary_distribution:Amino_acid.vector ->
    (_, Amino_acid.t, float * int) Tree.t ->
    float * param

  val simulate_site :
    exchangeability_matrix:Amino_acid.matrix ->
    stationary_distribution:Amino_acid.vector ->
    ('a, 'b, float * int) Tree.t ->
    param:param ->
    simulation

  val likelihood_plot_demo : Wag.t -> unit
end

module Model2 : sig
  type param = {
    scale : float ;
    stationary_distribution : Amino_acid.vector ;
  }

  val maximum_log_likelihood :
    ?debug:bool ->
    ?mode:[< `dense | `sparse > `sparse ] ->
    exchangeability_matrix:Rate_matrix.Amino_acid.t ->
    (_, Amino_acid.t, float * int) Tree.t ->
    float * param
end

module Model3 : sig
  type param = {
    scale : float ;
    stationary_distribution0 : Amino_acid.vector ;
    stationary_distribution1 : Amino_acid.vector ;
  }

  val maximum_log_likelihood :
    ?debug:bool ->
    ?mode:[< `dense | `sparse > `sparse ] ->
    exchangeability_matrix:Rate_matrix.Amino_acid.t ->
    Convsim.tree ->
    (_, Amino_acid.t, float * int) Tree.t ->
    float * param

  val simulate_site :
    Amino_acid.matrix ->
    (_, _, float * int) Tree.t ->
    float ->
    Amino_acid.vector ->
    Amino_acid.vector ->
    simulation
end

val lrt_1_vs_2_null_simulation :
  ?seed:int ->
  ?mode:[< `dense | `sparse > `sparse ] ->
  Wag.t ->
  (simulation * Model1.param * Model2.param * likelihood_ratio_test) array

val lrt_2_vs_3_null_simulation :
  ?seed:int ->
  ?mode:[< `dense | `sparse > `sparse ] ->
  ?alpha:float ->
  ?nb_simulations:int ->
  Wag.t ->
  (simulation * Model2.param * Model3.param * likelihood_ratio_test) array
