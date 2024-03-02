module RM = Rate_matrix.Nucleotide

type t =
  | JC69
  | K80 of float
  | HKY85 of {
      stationary_distribution : Nucleotide.vector ;
      transition_rate : float ;
      transversion_rate : float ;
    }
  | GTR of {
      stationary_distribution : Nucleotide.vector ;
      exchangeabilities : Nucleotide.matrix ;
    }

let rate_matrix = function
  | JC69 -> RM.jc69 ()
  | K80 kappa -> RM.k80 kappa
  | HKY85 { stationary_distribution ; transversion_rate ; transition_rate } ->
    RM.hky85 ~stationary_distribution ~transition_rate ~transversion_rate
  | GTR { stationary_distribution ; exchangeabilities } ->
    RM.gtr ~stationary_distribution ~exchangeabilities

let stationary_distribution = function
  | JC69
  | K80 _ -> Nucleotide.Vector.init (fun _ -> 0.25)
  | HKY85 { stationary_distribution ; _ }
  | GTR { stationary_distribution ; _ } -> stationary_distribution

module Random = struct
  let gtr rng ~alpha =
    let stationary_distribution = Nucleotide.random_profile rng alpha in
    let exchangeabilities = Rate_matrix.Nucleotide.make_symetric (fun _ _ -> Gsl.Randist.gamma rng ~a:1. ~b:1.) in
    GTR { stationary_distribution ; exchangeabilities }

  let hky85 rng ~alpha =
    let stationary_distribution = Nucleotide.random_profile rng alpha in
    let transition_rate = Gsl.Randist.gamma rng ~a:1. ~b:1. in
    let transversion_rate = Gsl.Randist.gamma rng ~a:1. ~b:1. in
    HKY85 { stationary_distribution ; transversion_rate ; transition_rate }
end
