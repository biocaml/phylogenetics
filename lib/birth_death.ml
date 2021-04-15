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
  let birth_mean_time = 1. /. p.birth_rate
  and death_mean_time = 1. /. p.death_rate in
  let rec branch remaining_time =
    let next_birth = Randist.exponential rng ~mu:birth_mean_time in
    let next_death = Randist.exponential rng ~mu:death_mean_time in
    let next_event = Float.min next_birth next_death in
    let* id = new_id in
    if Float.(next_event > remaining_time) then
      return @@ Tree.branch remaining_time (Tree.leaf id)
    else if Float.(next_birth < next_death) then
      let remaining_time' = remaining_time -. next_event in
      let* left_branch = branch remaining_time' in
      let+ right_branch = branch remaining_time' in
      Tree.branch next_birth (Tree.binary_node id left_branch right_branch)
    else
      return @@ Tree.branch next_death (Tree.leaf id)
  in
  run (branch time)
