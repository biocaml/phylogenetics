type param = {
  birth_rate : float ;
  death_rate : float ;
}

val simulate :
  param ->
  float ->
  (int, int, float) Tree.branch
