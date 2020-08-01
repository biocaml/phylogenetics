open Core_kernel
module Convsim = Simulator
open Phylogenetics
open Phylogenetics.Linear_algebra.Lacaml


module Evolution_model = struct
  type param = {
    stationary_distribution : Amino_acid.vector ;
    exchangeability_matrix : Amino_acid.matrix ;
    scale : float ;
  }
  let param_of_wag (wag : Wag.t) scale = {
    scale ;
    stationary_distribution = wag.freqs ;
    exchangeability_matrix = wag.rate_matrix ;
  }
  let stationary_distribution p = p.stationary_distribution
  let rate_matrix p =
    Rate_matrix.Amino_acid.make (fun i j ->
        p.scale *.
        p.exchangeability_matrix.Amino_acid.%{i, j} *.
        p.stationary_distribution.Amino_acid.%(j)
      )
  let transition_probability_matrix p t =
    Amino_acid.Matrix.(expm (scal_mul t (rate_matrix p)))
end

module Simulator = Simulator.Make(Amino_acid)(Evolution_model)

let choose_aa p =
  Amino_acid.Table.of_vector p
  |> Amino_acid.Table.choose

module CTMC = Phylo_ctmc.Make(Amino_acid)

module Model1 = struct

  let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site =
    let pi = (stationary_distribution : Amino_acid.vector :> vec) in
    let p = { Evolution_model.scale = 10. ** scale ;
              exchangeability_matrix ;
              stationary_distribution } in
    let transition_matrix =
      let f = Evolution_model.transition_probability_matrix p in
      fun (bl, _) -> (f bl :> mat)
    in
    CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:pi

  let maximum_likelihood ?debug ~exchangeability_matrix ~stationary_distribution site =
    let f param = -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale:param.(0) site in
    let sample () = [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |] in
    let ll, p_star = Nelder_mead.minimize ?debug ~tol:0.01 ~maxit:10_000 ~f ~sample () in
    ll, p_star.(0)

  let simulate_site exchangeability_matrix stationary_distribution tree scale =
    let root = choose_aa stationary_distribution in
    let p = {
      Evolution_model.stationary_distribution ; scale ;
      exchangeability_matrix ;
    }
    in
    Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p)

  let demo (wag : Wag.t) =
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100 in
    let root = choose_aa wag.freqs in
    let true_scale = 1. in
    let p = Evolution_model.param_of_wag wag true_scale in
    let site = Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p) in
    let ll, scale_hat =
      maximum_likelihood
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        site
    in
    let f scale =
      log_likelihood
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        ~scale
        site
    in
    let x = Array.init 100 ~f:(fun i ->
        let i = float i in
        let a = -1. and b = 2. in
        a +. (b -. a) *. i /. 100.
      )
    in
    let y = Array.map x ~f in
    printf "LL = %g, scale_hat = %g" ll scale_hat ;
    OCamlR_graphics.plot ~x ~y ()

  module Sample = struct
    let draw ~exchangeability_matrix ~stationary_distribution ~scale tree n =
      List.init n ~f:(fun _ ->
          simulate_site exchangeability_matrix stationary_distribution tree scale
        )

    let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale sample =
      List.fold sample ~init:0. ~f:(fun acc site ->
          acc +. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
        )

    let maximum_likelihood ?debug ~exchangeability_matrix ~stationary_distribution sample =
      let f param = -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale:param.(0) sample in
      let sample () = [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |] in
      let ll, p_star = Nelder_mead.minimize ?debug ~tol:0.01 ~maxit:10_000 ~f ~sample () in
      -. ll, p_star.(0)
  end

end

module Model2 = struct
  let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site =
    let p = { Evolution_model.scale ; exchangeability_matrix ; stationary_distribution } in
    let transition_matrix =
      let f = Evolution_model.transition_probability_matrix p in
      fun (bl, _) -> (f bl :> mat)
    in
    CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:(stationary_distribution :> Vector.t)

  let counts xs =
    Amino_acid.Table.init (fun aa -> List.count xs ~f:(Amino_acid.equal aa))

  type param_schema = {
    nz : int ; (* number of non-zero AA in count table *)
    idx : int array ; (* indices of non-zero AA *)
  }

  let sparse_param_schema counts =
    let k = (counts : int Amino_acid.table :> _ array) in
    let idx, nz = Array.foldi k ~init:([], 0) ~f:(fun i ((assoc, nz) as acc) k_i ->
        if k_i = 0 then acc else i :: assoc, nz + 1
      )
    in
    let idx = Array.of_list idx in
    { nz ; idx }

  let dense_param_schema counts =
    let nz = Array.length (counts : int Amino_acid.table :> _ array) in
    let idx = Array.init nz ~f:Fn.id in
    { nz ; idx }

  let profile_guess schema counts =
    let counts = (counts : int Amino_acid.table :> _ array) in
    let total_counts = Array.fold counts ~init:0. ~f:(fun acc x -> 1. +. acc +. float x) in
    Array.map schema.idx ~f:(fun idx -> Float.log (float (1 + counts.(idx)) /. total_counts))

  let initial_param schema counts =
    Array.append [| 0. |] (profile_guess schema counts)

  let extract_frequencies ~offset schema param =
    let r = Array.create ~len:Amino_acid.card 0. in
    Array.iteri schema.idx ~f:(fun sparse_idx full_idx ->
        r.(full_idx) <- Float.exp param.(sparse_idx + offset)
      ) ;
    let s = Owl.Stats.sum r in
    Amino_acid.Vector.init (fun aa -> r.((aa :> int)) /. s)

  let param_schema ?(mode = `sparse) counts =
    match mode with
    | `sparse -> sparse_param_schema counts
    | `dense  -> dense_param_schema counts

  let nelder_mead_init theta0 =
    let c = ref (-1) in
    fun _ ->
      incr c ;
      if !c = 0 then theta0
      else
        Array.init (Array.length theta0) ~f:(fun i ->
            theta0.(i) +. if i = !c - 1 then  -. 1. else 0.
          )

  let maximum_log_likelihood ?debug ?mode ~exchangeability_matrix site =
    let counts = counts (Tree.leaves site) in
    let schema = param_schema ?mode counts in
    let theta0 = initial_param schema counts in
    let sample = nelder_mead_init theta0 in
    let f param =
      let stationary_distribution = extract_frequencies ~offset:1 schema param in
      let scale = 10. ** param.(0) in
      -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
    in
    let ll, p_star = Nelder_mead.minimize ~tol:0.01 ?debug ~maxit:10_000 ~f ~sample () in
    -. ll, p_star

  let simulate_profile () =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:Amino_acid.card 0.1)
    |> Amino_acid.Vector.of_array_exn

  let demo ?debug (wag : Wag.t) =
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100 in
    let true_pi = simulate_profile () in
    let true_scale = 1. in
    let site = Model1.simulate_site wag.rate_matrix true_pi tree true_scale in
    let ll_star = log_likelihood ~exchangeability_matrix:wag.rate_matrix ~stationary_distribution:true_pi ~scale:true_scale site in
    printf "LL* = %g\n" ll_star ;
    let ll, p_hat =
      maximum_log_likelihood ?debug ~exchangeability_matrix:wag.rate_matrix site
    in
    printf "LL = %g, scale_hat = %g\n" ll p_hat.(0)

  module Sample = struct
    let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale sample =
      List.fold sample ~init:0. ~f:(fun acc site ->
          acc +. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
        )

    let maximum_likelihood ?debug ?mode ~exchangeability_matrix sample =
      let counts =
        List.map sample ~f:(fun site -> counts (Tree.leaves site))
        |> List.reduce_exn ~f:(fun k1 k2 ->
            Amino_acid.Table.(init (fun aa -> get k1 aa + get k2 aa))
          )
      in
      let schema = param_schema ?mode counts in
      let theta0 = initial_param schema counts in
      let f param =
        let stationary_distribution = extract_frequencies ~offset:1 schema param in
        let scale = 10. ** param.(0) in
        -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale sample
      in
      let sample = nelder_mead_init theta0 in
      let ll, p_star = Nelder_mead.minimize ?debug ~tol:0.1 ~maxit:10_000 ~f ~sample () in
      -. ll, p_star.(0)
  end
end

module Model3 = struct
  let param exchangeability_matrix scale pi0 pi1 cond =
    let f stationary_distribution =
      { Evolution_model.scale ; exchangeability_matrix ; stationary_distribution }
    in
    match cond with
    | 0 -> f pi0
    | 1 -> f pi1
    | _ -> assert false

  let log_likelihood ~exchangeability_matrix ~stationary_distribution:(pi0, pi1) ~scale site =
    let param = param exchangeability_matrix scale pi0 pi1 in
    let p0 = param 0 in
    let p1 = param 1 in
    let transition_matrix =
      let f0 = Evolution_model.transition_probability_matrix p0 in
      let f1 = Evolution_model.transition_probability_matrix p1 in
      fun (bl, cond) ->
        match cond with
        | 0 -> (f0 bl :> mat)
        | 1 -> (f1 bl :> mat)
        | _ -> assert false
    in
    CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:(pi0 :> Vector.t)

  let tuple_map (x, y) ~f = (f x, f y)

  let counts (tree : Convsim.tree) site =
    List.zip_exn
      (Tree.leaves tree)
      (Tree.leaves site)
    |> List.partition_tf ~f:(fun ((_, i), _) -> i = 0)
    |> tuple_map ~f:(List.map ~f:snd)
    |> tuple_map ~f:Model2.counts

  let initial_param schema tree site =
    let k0, k1 = counts tree site in
    Array.concat [
      [| 0. |] ;
      Model2.profile_guess schema k0 ;
      Model2.profile_guess schema k1 ;
    ]

  let extract_frequencies schema param =
    Model2.extract_frequencies ~offset:1 schema param,
    Model2.extract_frequencies ~offset:(1 + schema.nz) schema param

  let maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site =
    let schema = Model2.param_schema ?mode (Model2.counts (Tree.leaves site)) in
    let theta0 = initial_param schema tree site in
    let f param =
      let stationary_distribution = extract_frequencies schema param in
      let scale = 10. ** param.(0) in
      -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
    in
    let sample best_guess =
      let c = ref (-1) in
      fun _ ->
        incr c ;
        if !c = 0 then best_guess
        else
          Array.init (Array.length theta0) ~f:(fun i ->
              best_guess.(i) +. if i = !c - 1 then  -. 1. else 0.
            )
    in
    let ll, p_star = Nelder_mead.minimize ~tol:0.1 ?debug ~maxit:10_000 ~f ~sample:(sample theta0) () in
    -. ll, p_star

  let simulate_site exchangeability_matrix tree scale pi0 pi1 =
    let param = param exchangeability_matrix scale pi0 pi1 in
    let root = choose_aa pi0 in
    Simulator.site_gillespie_first_reaction tree ~root ~param

  let demo ?debug (wag : Wag.t) =
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100 in
    let true_pi0 = Model2.simulate_profile () in
    let true_pi1 = Model2.simulate_profile () in
    let true_scale = 1. in
    let stationary_distribution = true_pi0, true_pi1 in
    let site = simulate_site wag.rate_matrix tree true_scale true_pi0 true_pi1 in
    let ll_star = log_likelihood ~exchangeability_matrix:wag.rate_matrix ~stationary_distribution ~scale:true_scale site in
    printf "LL* = %g\n" ll_star ;
    let ll, p_hat =
      maximum_log_likelihood ?debug ~exchangeability_matrix:wag.rate_matrix tree site
    in
    printf "LL = %g, scale_hat = %g\n" ll p_hat.(0)

  module Sample = struct
    let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale sample =
      List.fold sample ~init:0. ~f:(fun acc site ->
          acc +. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
        )

    let add_counts k1 k2 =
      Amino_acid.Table.(init (fun aa -> get k1 aa + get k2 aa))

    let initial_param schema tree samples =
      let k0, k1 =
        List.map samples ~f:(counts tree)
        |> List.reduce_exn ~f:(fun (k00, k01) (k10, k11) ->
            add_counts k00 k10,
            add_counts k01 k11
          )
      in
      Array.concat [
        [| 0. |] ;
        Model2.profile_guess schema k0 ;
        Model2.profile_guess schema k1 ;
      ]

    let maximum_likelihood ?debug ?mode ~exchangeability_matrix tree sample =
      let counts =
        List.map sample ~f:(fun site -> Model2.counts (Tree.leaves site))
        |> List.reduce_exn ~f:add_counts
      in
      let schema = Model2.param_schema ?mode counts in
      let theta0 = initial_param schema tree sample in
      let f param =
        let stationary_distribution = extract_frequencies schema param in
        let scale = 10. ** param.(0) in
        -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale sample
      in
      let sample = Model2.nelder_mead_init theta0 in
      let ll, p_star = Nelder_mead.minimize ?debug ~tol:0.1 ~maxit:10_000 ~f ~sample () in
      -. ll, p_star.(0)
  end
end

let lrt_1_vs_2_null_demo ~sample_size (wag : Wag.t) =
  let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:30 in
  let true_scale = 1. in
  let f i =
    printf "Iteration %d:\n%!" i ;
    let sample =
      Model1.Sample.draw
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        ~scale:true_scale tree
        sample_size in
    let model1_ll, _ = Model1.Sample.maximum_likelihood ~debug:true ~exchangeability_matrix:wag.rate_matrix ~stationary_distribution:wag.freqs sample in
    let model2_ll, _ = Model2.Sample.maximum_likelihood ~debug:true ~mode:`dense ~exchangeability_matrix:wag.rate_matrix sample in
    2. *. (model2_ll -. model1_ll)
  in
  let sample = Array.init 1_000 ~f in
  ignore (
    OCamlR_graphics.hist
      ~main:(sprintf "sample_size = %d" sample_size)
      ~xlab:"2 log(L1 / L2)"
      ~freq:false
      ~breaks:(`n 20) sample :> OCamlR_graphics.hist) ;
  let x = Array.init 100 ~f:(fun i -> float i) in
  let y = Array.map x ~f:(Gsl.Randist.chisq_pdf ~nu:19.) in
  OCamlR_graphics.lines ~x ~y ()

let lrt_2_vs_3_null_demo ~sample_size (wag : Wag.t) =
  let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:30 in
  let true_scale = 1. in
  let f i =
    printf "Iteration %d:\n%!" i ;
    let sample =
      Model1.Sample.draw
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        ~scale:true_scale tree
        sample_size in
    let model2_ll, _ = Model2.Sample.maximum_likelihood ~debug:true ~mode:`dense ~exchangeability_matrix:wag.rate_matrix sample in
    let model3_ll, _ = Model3.Sample.maximum_likelihood ~debug:true ~mode:`dense ~exchangeability_matrix:wag.rate_matrix tree sample in
    2. *. (model3_ll -. model2_ll)
  in
  let stat_values = Array.init 10 ~f in
  ignore (
    OCamlR_graphics.hist
      ~main:(sprintf "sample_size = %d" sample_size)
      ~xlab:"2 log(L1 / L2)"
      ~freq:false
      ~breaks:(`n 20) stat_values :> OCamlR_graphics.hist) ;
  let x = Array.init 100 ~f:(fun i -> float i) in
  let y = Array.map x ~f:(Gsl.Randist.chisq_pdf ~nu:19.) in
  OCamlR_graphics.lines ~x ~y ()


let lrt_2_3 ?debug ?mode exchangeability_matrix tree site =
  let model2_ll, p2 =
    Model2.maximum_log_likelihood ?debug ?mode ~exchangeability_matrix site
  in
  let model3_ll, p3 =
    Model3.maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site
  in
  let _D_ = 2. *. (model3_ll -. model2_ll) in
  let df = float (Array.length p3 - Array.length p2 - 1) in
  let pvalue = 1. -. Owl.Stats.chi2_cdf ~df _D_ in
  _D_, df, pvalue

let lrt_null_demo ?(sample_size = 1_000) (wag : Wag.t) =
  let sample _ =
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:30 in
    let true_pi = Model2.simulate_profile () in
    let true_scale = 1. in
    let site = Model1.simulate_site wag.rate_matrix true_pi tree true_scale in
    let _D_, df, pval = lrt_2_3 wag.rate_matrix tree site in
    printf "D = %f, df = %f, p = %f\n%!" _D_ df pval ;
    pval
  in
  let pvals = Array.init sample_size ~f:sample in
  ignore (OCamlR_graphics.hist ~breaks:(`n 20) pvals :> OCamlR_graphics.hist)

let lrt_demo (wag : Wag.t) =
  let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100 in
  let true_pi0 = Model2.simulate_profile () in
  let true_pi1 = Model2.simulate_profile () in
  let true_scale = 1. in
  let site = Model3.simulate_site wag.rate_matrix tree true_scale true_pi0 true_pi1 in
  let model2_ll, _ =
    Model2.maximum_log_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      site
  in
  let model3_ll, _ =
    Model3.maximum_log_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      tree
      site
  in
  printf "model2 LL = %g, model3 LL = %g\n" model2_ll model3_ll
