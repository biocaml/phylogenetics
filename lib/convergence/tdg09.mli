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

type likelihood_ratio_test = {
  full_log_likelihood : float ;
  reduced_log_likelihood : float ;
  _D_ : float ;
  df : float ;
  pvalue : float ;
}

module type S = sig
  type branch_info
  type leaf_info
  type site

  type simulation = (Amino_acid.t, Amino_acid.t, branch_info) Tree.t

  module Model1 : sig
    type param = float

    val maximum_log_likelihood :
      ?debug:bool ->
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      stationary_distribution:Amino_acid.vector ->
      (_, leaf_info, branch_info) Tree.t ->
      site ->
      float * param

    val simulate_site :
      exchangeability_matrix:Amino_acid.matrix ->
      stationary_distribution:Amino_acid.vector ->
      (_, _, branch_info) Tree.t ->
      param:param ->
      simulation
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
      (_, leaf_info, branch_info) Tree.t ->
      site ->
      float * param

    val lrt :
      ?mode:[< `dense | `sparse > `sparse ] ->
      Wag.t ->
      (_, leaf_info, branch_info) Tree.t ->
      site ->
      Model1.param * param * likelihood_ratio_test
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
      (_, leaf_info, branch_info) Tree.t ->
      site ->
      float * param

    val lrt :
      ?mode:[< `dense | `sparse > `sparse ] ->
      Wag.t ->
      (_, leaf_info, branch_info) Tree.t ->
      site ->
      Model2.param * param * likelihood_ratio_test

    val simulate_site :
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      scale:float ->
      stationary_distribution0:Amino_acid.vector ->
      stationary_distribution1:Amino_acid.vector ->
      (_, leaf_info, branch_info) Tree.t ->
      simulation
  end
end

module type Leaf_info = sig
  type t
  type species
  val species : t -> species
  val condition : t -> [`Ancestral | `Convergent]
end

module type Branch_info = sig
  type t
  val length : t -> float
  val condition : t -> [`Ancestral | `Convergent]
end

module type Site = sig
  type t
  type species
  val get_aa : t -> species -> Amino_acid.t
end

module Make(Branch_info : Branch_info)(Leaf_info : Leaf_info)(Site : Site with type species = Leaf_info.species) :
  S with type site := Site.t
     and type leaf_info := Leaf_info.t
     and type branch_info := Branch_info.t

module Implementation_check : sig
  include S with type site := Amino_acid.t array
             and type leaf_info := int * Convsim.condition
             and type branch_info := Convsim.Branch_info.t

  val simulate_profile : float -> Amino_acid.vector

  val likelihood_plot_demo : Wag.t -> unit

  val lrt_1_vs_2_null_simulation :
    ?seed:int ->
    ?mode:[< `dense | `sparse > `sparse ] ->
    ?nb_simulations:int ->
    Wag.t ->
    (simulation * Model1.param * Model2.param * likelihood_ratio_test) array

  val lrt_2_vs_3_null_simulation :
    ?seed:int ->
    ?mode:[< `dense | `sparse > `sparse ] ->
    ?alpha:float ->
    ?nb_simulations:int ->
    Wag.t ->
    (simulation * Model2.param * Model3.param * likelihood_ratio_test) array

  val render_pvalue_histogram :
    title:string ->
    (_ * _ * _ * likelihood_ratio_test) array ->
    string ->
    unit

  val render_stat_histogram :
    title:string ->
    df:float ->
    (_ * _ * _ * likelihood_ratio_test) array ->
    string ->
    unit
end
