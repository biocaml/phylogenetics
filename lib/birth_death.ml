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

let sample_different_ints rng n =
  let i = Rng.uniform_int rng n in
  let j = Rng.uniform_int rng (n - 1) in
  let j = if j < i then j else j + 1 in
  if i < j then i, j
  else j, i

let sample_branch rng times =
  let n = Array.length times + 1 in
  let particles = Array.init n ~f:(fun i -> Tree.leaf i, 0.) in
  let rec loop i =
    if i = 1 then fst particles.(0)
    else
      let k, l = sample_different_ints rng i in
      let t = times.(n - i) in
      let length_k = t -. snd particles.(k) in
      let branch_k = Tree.branch length_k (fst particles.(k)) in
      let length_l = t -. snd particles.(l) in
      let branch_l = Tree.branch length_l (fst particles.(l)) in
      particles.(k) <- (Tree.binary_node () branch_k branch_l), t ;
      particles.(l) <- particles.(i - 1) ;
      loop (i - 1)
  in
  loop n

(* TODO: check the implementation against the TESS R package, for
 * instance by inspecting the distribution of a summary statistics
 * like the gamma on simulation from both implementations *)
let age_ntaxa_simulation ?sampling_probability:(rho = 1.) p rng ~age ~ntaxa =
  if Float.(p.birth_rate <= p.death_rate) then invalid_arg "expected birth_rate > death_rate" ;
  let n_inner_nodes = ntaxa - 1 in
  if n_inner_nodes < 1 then invalid_arg "not enough taxa" ;
  let u = Array.init n_inner_nodes ~f:(fun _ -> Rng.uniform rng) in
  let b = p.birth_rate and d = p.death_rate in
  let speciation_times = Array.map u ~f:Float.(fun u ->
      age
      -
      (
        log (
          (
            (b - d)
            / (1. - u * (1. - ((b-d)*exp((d-b)*age))/(rho*b+(b*(1.-rho)-d)*exp((d-b)*age) ) ) )
            - (b * (1. - rho) - d)
          )
          /
          (rho * b)
        )
        + (d - b) * age ) / (d-b)
    )
  in
  Array.sort speciation_times ~compare:Float.compare ;
  sample_branch rng speciation_times
