(** Updatable discrete probability distribution

    This module provides functionality for working with discrete
    probability distributions.  A discrete probability distribution
    (DPD) represents the probability distribution of a discrete random
    variable, where each possible value (category) has an associated
    probability weight.

    The DPD in this module supports operations such as initialization,
    updating category weights, sampling categories according to their
    probabilities, and calculating the total weight of the
    distribution.

    The DPD module is designed to be used in various applications that
    involve discrete probability distributions, such as stochastic
    simulations, Markov chain Monte Carlo (MCMC) methods, and
    probabilistic modeling.

    Example usage:

    {[
      let dpd = Discrete_pd.init 3 ~f:(fun i -> float (i + 1))
          Discrete_pd.update dpd 1 4.0
      let rng = Gsl.Rng.(make (default ()))
      let category = Discrete_pd.draw dpd rng
    ]}

    In the example above, a discrete probability distribution (dpd)
    with 3 categories is initialized, and the weight of category 1 is
    updated. Then, a category index is sampled using the DPD and a
    random number generator (rng).
*)

type t

val init : int -> f:(int -> float) -> t
(** [init n ~f] initializes an updatable discrete probability
    distribution with [n] categories.  It takes a function [f] that
    specifies the weight for each category.

    Example:
    {[
      let dpd = init 3 ~f:(fun i -> float (i + 1))
    ]}

    This initializes a discrete probability distribution with 3
    categories and assigns weights 1.0, 2.0, and 3.0 to each category
    respectively. *)

val update : t -> int -> float -> unit
(** [update dpd i w_i] updates the weight of category [i] in the
    discrete probability distribution [dpd] to the new weight [w_i].

    Example:
    {[
      let dpd = init 3 ~f:(fun i -> float (i + 1))
          update dpd 1 4.0
    ]}

    This updates the weight of category 1 in the discrete probability
    distribution [dpd] to 4.0. *)

val draw : t -> Gsl.Rng.t -> int
(** [draw dpd rng] samples a category index from the discrete
    probability distribution [dpd] using the random number generator
    [rng]. It returns the index of the sampled category.

    Example:
    {[
      let dpd = init 3 ~f:(fun i -> float (i + 1))
      let rng = Gsl.Rng.(make (default ()))
      let category = draw dpd rng
    ]}
 *)

val total_weight : t -> float
(** [total_weight dpd] returns the total weight of all categories in
    the discrete probability distribution [dpd].

    Example:
    {[
      let dpd = init 3 ~f:(fun i -> float (i + 1))
      let weight = total_weight dpd
    ]}

    This calculates the total weight of all categories in the discrete
    probability distribution [dpd] and assigns it to the variable
    [weight]. *)

val demo : n:int -> ncat:int -> float array * float array
(** [demo ~n ~ncat] is a demo function that generates a discrete
    probability distribution with [ncat] categories and generates a
    sample of size [n] from it. It returns a tuple formed by the
    probability distribution and the frequencies in the sample.

    Example:
    {[
      let probabilities, counts = demo ~n:1000 ~ncat:5
    ]}

    This generates random probabilities, creates an updatable discrete
    probability distribution with 5 categories, performs 1000 sampling
    trials, and assigns the generated probabilities normalized by
    their sum to the variable [probabilities] and the counts of each
    sampled category divided by 1000 to the variable [counts]. *)
