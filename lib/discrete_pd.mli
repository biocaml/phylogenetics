(* Updatable discrete probability distribution *)

type t

val init : int -> f:(int -> float) -> t

val update : t -> int -> float -> unit

val draw : t -> Gsl.Rng.t -> int

val total_weight : t -> float

val demo : n:int -> ncat:int -> float array * float array
