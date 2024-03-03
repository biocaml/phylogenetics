open Core

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

module Make(A : Alphabet) = struct
  type profile = Profile of {
      probs : float array ;
      dist : Gsl.Randist.discrete ;
    }

  let profile_of_array_exn probs =
    if Array.length probs <> A.card then failwith "Array has incorrect size" ;
    let dist = Gsl.Randist.discrete_preproc probs in
    Profile { probs ; dist }

  let random_profile ~alpha rng =
    let alpha = Array.create ~len:A.card alpha in
    let theta = Array.create ~len:A.card 0. in
    Gsl.Randist.dirichlet rng ~alpha ~theta ;
    profile_of_array_exn theta

  let draw_from_profile (Profile p) rng =
    Gsl.Randist.discrete rng p.dist
    |> A.of_int_exn

  type pwm = profile array

  let random_pwm ~alpha n rng =
    Array.init n ~f:(fun _ -> random_profile ~alpha rng)

  let draw_from_pwm pwm rng =
    let n = Array.length pwm in
    String.init n ~f:(fun i ->
        draw_from_profile pwm.(i) rng
        |> A.to_char
      )

  let vec_of_profile (Profile p) =
    A.Vector.of_array_exn p.probs
end
