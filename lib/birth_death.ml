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

(** A state monad to generate integer identifiers *)
module Id_monad = struct
  type 'a t = int -> 'a * int

  let (let*) (x : 'a t) (f : 'a -> 'b t) : 'b t  =
    fun state ->
    let y, state' = x state in
    f y state'

  let (let+) (x : 'a t) (f : 'a -> 'b) : 'b t  =
    fun state ->
    let y, state' = x state in
    f y, state'

  let new_id id = (id, id + 1)
end

let simulation p rng ~time =
  let open Id_monad in
  let rec branch next_id t =
    let next_birth = Randist.exponential rng ~mu:p.birth_rate in
    let next_death = Randist.exponential rng ~mu:p.death_rate in
    let next_event = Float.min next_birth next_death in
    if Float.(t +. next_event > time) then
      let+ id = new_id in
      Tree.branch (time -. t) (Tree.leaf id)
    else if Float.(next_birth < next_death) then
      let* left_id = new_id in
      let* right_id = new_id in
      let* left_branch = branch left_id (t -. next_birth) in
      let+ right_branch = branch right_id (t -. next_birth) in
      Tree.branch next_birth (Tree.binary_node next_id left_branch right_branch)
    else
      let+ id = new_id in
      Tree.branch next_death (Tree.leaf id)
  in
  let k = branch 0 0. in
  fst (k 0)
