open Base
open Gsl

type t = {
  birth_rate : float ;
  death_rate : float ;
}

let make ~birth_rate ~death_rate =
  if Float.(birth_rate < 0. || death_rate < 0.)
  then invalid_arg "birth rate and death rate should be positive" ;
  { birth_rate ; death_rate }

let simulation p rng ~time =
  let open ID_monad in
  let rec branch t =
    let next_birth = Randist.exponential rng ~mu:p.birth_rate in
    let next_death = Randist.exponential rng ~mu:p.death_rate in
    let next_event = Float.min next_birth next_death in
    let* id = new_id in
    if Float.(t +. next_event > time) then
      return @@ Tree.branch (time -. t) (Tree.leaf id)
    else if Float.(next_birth < next_death) then
      let* left_branch = branch (t -. next_birth) in
      let+ right_branch = branch (t -. next_birth) in
      Tree.branch next_birth (Tree.binary_node id left_branch right_branch)
    else
      return @@ Tree.branch next_death (Tree.leaf id)
  in
  run (branch 0.)
