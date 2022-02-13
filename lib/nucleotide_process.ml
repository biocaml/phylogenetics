open Linear_algebra

module RM = Rate_matrix.Nucleotide

type t =
  | JC69
  | K80 of float
  | HKY85 of {
      equilibrium_frequencies : Nucleotide.vector ;
      transition_rate : float ;
      transversion_rate : float ;
    }
  | GTR of {
      equilibrium_frequencies : Nucleotide.vector ;
      exchangeabilities : Linear_algebra.vec
    }

let rate_matrix = function
  | JC69 -> RM.jc69 ()
  | K80 kappa -> RM.k80 kappa
  | HKY85 { equilibrium_frequencies ; transversion_rate ; transition_rate } ->
    RM.hky85 ~equilibrium_frequencies ~transition_rate ~transversion_rate
  | GTR { equilibrium_frequencies ; exchangeabilities } ->
    RM.gtr ~equilibrium_frequencies ~transition_rates:exchangeabilities

let stationary_distribution = function
  | JC69
  | K80 _ -> Nucleotide.Vector.init (fun _ -> 0.25)
  | HKY85 { equilibrium_frequencies ; _ }
  | GTR { equilibrium_frequencies ; _ } -> equilibrium_frequencies

module Random = struct
  let gtr rng ~alpha =
    let equilibrium_frequencies = Nucleotide.random_profile rng alpha in
    let exchangeabilities = Vector.init 6 ~f:(fun _ -> Gsl.Randist.gamma rng ~a:1. ~b:1.) in
    GTR { equilibrium_frequencies ; exchangeabilities }

  let hky85 rng ~alpha =
    let equilibrium_frequencies = Nucleotide.random_profile rng alpha in
    let transition_rate = Gsl.Randist.gamma rng ~a:1. ~b:1. in
    let transversion_rate = Gsl.Randist.gamma rng ~a:1. ~b:1. in
    HKY85 { equilibrium_frequencies ; transversion_rate ; transition_rate }
end
