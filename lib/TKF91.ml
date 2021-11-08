open Core_kernel

module Make_simulator
    (A : Alphabet.S_int)
    (BI : Simulator.Branch_info) =
struct
  let symbol_sample rng v =
    Gsl.Randist.discrete_preproc v
    |> Gsl.Randist.discrete rng
    |> A.of_int_exn

  type site = Site of (A.t, (A.t * int) option, branch_info) Tree.t
  and branch_info = {
    original_branch_info : BI.t ;
    length : float ;
    insertions : (A.t, (A.t * int) option, branch_info) Tree.branch list
  }

  let fold_alignment (Site site) ~init ~f =
    let open Tree in
    let rec node col ((next_col, acc) as acc2) = function
      | Leaf (Some (state, row)) -> next_col, f acc ~row ~col state
      | Leaf None -> acc2
      | Node n ->
        List1.fold n.branches ~init:acc2 ~f:(branch col)

    and branch col acc2 (Branch b) =
      List.fold b.data.insertions ~init:(node col acc2 b.tip) ~f:(fun (next_col, acc) b ->
          branch next_col (next_col + 1, acc) b
        )
    in
    snd (node 0 (1, init) site)

  let nb_alignment_columns site =
    fold_alignment site ~init:0 ~f:(fun prev_col ~row:_ ~col _ -> max col prev_col) + 1

  let alignment_of_site char_of_state tree site =
    let ncols = nb_alignment_columns site in
    let nrows = List.length (Tree.leaves tree) in
    let ali = Array.init nrows ~f:(fun _ -> Bytes.init ncols ~f:(fun _ -> '-')) in
    fold_alignment site ~init:() ~f:(fun () ~row ~col state ->
        Bytes.set ali.(row) col (char_of_state state)
      ) ;
    Array.map ali ~f:Bytes.to_string

  let index_leaves tree =
    let open Tree in
    let rec node state = function
      | Node n ->
        let state, branches =
          List1.fold n.branches ~init:(state, []) ~f:(fun (state, acc) b ->
              let state, b = branch state b in
              state, b :: acc
            )
        in
        state, Tree.node n.data (List1.of_list_exn branches)
      | Leaf l ->
        state + 1, Leaf (state, l)
    and branch state (Branch b) =
      let state, tip = node state b.tip in
      state, Tree.branch b.data tip
    in
    snd (node 0 tree)

  let site_gillespie_direct rng tree ~root ~rate_matrix ~rates_upon_insertion ~lambda ~mu =

    let simulation_step remaining_time state ~insertion ~deletion ~substitution ~rate_matrix ~rates_upon_insertion ~branch_end =
      let substitution_rates = A.Table.init (fun m -> if A.equal m state then 0. else rate_matrix.A.%{state, m}) in
      let total_substitution_rate = Utils.array_sum (substitution_rates :> float array) in
      let total_rate = total_substitution_rate +. lambda +. mu in
      let tau = Gsl.Randist.exponential rng ~mu:(1. /. total_rate) in
      let remaining_time = Float.(remaining_time - tau) in
      if Float.(remaining_time < 0.) then branch_end ()
      else
        let x = total_rate *. Gsl.Rng.uniform rng in
        if Float.(x < total_substitution_rate) then
          let next_state = symbol_sample rng (substitution_rates :> float array) in
          substitution remaining_time next_state
        else if Float.(x < total_substitution_rate +. lambda) then
          let next_state = symbol_sample rng (rates_upon_insertion :> float array) in
          insertion remaining_time next_state
        else
          deletion remaining_time
    in

    let rec simulate_tree : (_, _, BI.t) Tree.t -> A.t option -> site =
      fun tree maybe_state ->
        match tree, maybe_state with
        | Leaf (id, _), Some state -> Site (Tree.leaf (Some (state, id)))
        | _, None -> Site (Tree.leaf None)
        | Node n, Some state ->
          let branches = List1.map n.branches ~f:(fun b -> simulate_branch b state) in
          Site (Tree.node state branches)

    and simulate_branch : (_, _, BI.t) Tree.branch -> A.t -> (A.t, (A.t * int) option, branch_info) Tree.branch =
      fun (Branch b) state ->
        let branch_length = BI.length b.data in
        let rate_matrix = rate_matrix b.data in
        let rates_upon_insertion = rates_upon_insertion b.data in
        let rec loop remaining_time state acc =
          simulation_step remaining_time state
            ~rate_matrix
            ~rates_upon_insertion
            ~branch_end:(fun () -> Some state, 0., acc)
            ~substitution:(fun remaining_time next_state ->
                loop remaining_time next_state acc
              )
            ~insertion:(fun remaining_time next_state ->
                let insertions = simulate_insertion b.data b.tip rate_matrix rates_upon_insertion remaining_time next_state in
                loop remaining_time state (insertions @ acc)
              )
            ~deletion:(fun remaining_time -> None, branch_length -. remaining_time, acc)
        in
        let next_state, length, insertions = loop branch_length state [] in
        let Site tip = simulate_tree b.tip next_state in
        let bi = {
          original_branch_info = b.data ;
          length ; insertions ;
        }
        in
        Tree.branch bi tip

    and simulate_insertion (branch_data : BI.t) (rest_of_the_tree : _ Tree.t) rate_matrix rates_upon_insertion (remaining_time : float) (state : A.t) : _ Tree.branch list =
      simulation_step remaining_time state
        ~rate_matrix
        ~rates_upon_insertion
        ~branch_end:(fun () ->
            [
              let bi = {
                original_branch_info = branch_data ;
                length = remaining_time ;
                insertions = [] ;
              }
              in
              let Site tip = simulate_tree rest_of_the_tree (Some state) in
              Tree.branch bi tip
            ]
          )
        ~substitution:(fun remaining_time next_state -> simulate_insertion branch_data rest_of_the_tree rate_matrix rates_upon_insertion remaining_time next_state)
        ~insertion:(fun remaining_time next_state ->
            simulate_insertion branch_data rest_of_the_tree rate_matrix rates_upon_insertion remaining_time state
            @ simulate_insertion branch_data rest_of_the_tree rate_matrix rates_upon_insertion remaining_time next_state
          )
        ~deletion:(fun _ -> [])
    in
    simulate_tree (index_leaves tree) (Some root)
end
