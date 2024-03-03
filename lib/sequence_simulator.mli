(** Functions to simulate sequences *)

module type Alphabet = sig
  type t
  val card : int
  val of_int_exn : int -> t
  val to_char : t -> char
  module Vector : sig
    type t
    val of_array_exn : float array -> t
  end
end

module Make(A : Alphabet) : sig
  type profile
  val profile_of_array_exn : float array -> profile
  val random_profile : alpha:float -> Gsl.Rng.t -> profile
  val draw_from_profile : profile -> Gsl.Rng.t -> A.t
  val vec_of_profile : profile -> A.Vector.t

  type pwm = profile array
  val random_pwm : alpha:float -> int -> Gsl.Rng.t -> pwm
  val draw_from_pwm : pwm -> Gsl.Rng.t -> string
end
