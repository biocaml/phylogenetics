type t = private {
  birth_rate : float ;
  death_rate : float ;
}

val make : birth_rate:float -> death_rate:float -> t

val simulation :
  t ->
  Gsl.Rng.t ->
  time:float ->
  (int, int, float) Tree.branch
