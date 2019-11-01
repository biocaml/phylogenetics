type param = {
  birth_rate : float ;
  death_rate : float ;
}

let simulate p time =
  let rec branch next_id t =
    let next_birth = Owl.Stats.exponential_rvs ~lambda:p.birth_rate in
    let next_death = Owl.Stats.exponential_rvs ~lambda:p.death_rate in
    let next_event = Float.min next_birth next_death in
    if t +. next_event > time then
      Tree.branch (time -. t) (Tree.leaf next_id),
      next_id + 1
    else if next_birth < next_death then
      let left_branch, next_id' = branch (next_id + 1) (t -. next_birth) in
      let right_branch, next_id'' = branch next_id' (t -. next_birth) in
      Tree.branch next_birth (Tree.binary_node next_id left_branch right_branch),
      next_id''
    else
      Tree.branch next_death (Tree.leaf next_id),
      next_id + 1
  in
  fst (branch 0 0.)
