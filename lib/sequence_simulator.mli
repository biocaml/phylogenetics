(** Functions to simulate sequences *)

module type Alphabet = sig
  type t
  val card : int
  val of_int_exn : int -> t
  val to_char : t -> char
end

module Make(A : Alphabet) : sig
  type profile = Profile of {
      probs : float array ;
      dist : Gsl.Randist.discrete ;
    }
  val of_array_exn : float array -> profile
  val random_profile : alpha:float -> Gsl.Rng.t -> profile
  val draw_from_profile : profile -> Gsl.Rng.t -> A.t

  type pwm = profile array
  val random_pwm : alpha:float -> int -> Gsl.Rng.t -> pwm
  val draw_from_pwm : pwm -> Gsl.Rng.t -> string
end
