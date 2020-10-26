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
  let transition_probability_matrix p =
    let m = rate_matrix p in
    fun t ->
      Amino_acid.Matrix.(expm (scal_mul t m))
end

let choose_aa p =
  Amino_acid.Table.of_vector p
  |> Amino_acid.Table.choose

module CTMC = Phylo_ctmc.Make(Amino_acid)

let tol = 0.001

type likelihood_ratio_test = {
  full_log_likelihood : float ;
  reduced_log_likelihood : float ;
  _D_ : float ;
  df : float ;
  pvalue : float ;
}

let lrt ~full_log_likelihood ~reduced_log_likelihood ~df =
  let _D_ = 2. *. (full_log_likelihood -. reduced_log_likelihood) in
  let pvalue = 1. -. Owl.Stats.chi2_cdf ~df _D_ in
  { full_log_likelihood ; reduced_log_likelihood ; _D_ ; df ; pvalue }

module type S = sig
  type branch_info
  type site
  type tree

  type simulation = (Amino_acid.t, Amino_acid.t, branch_info) Tree.t

  module Model1 : sig
    type param = float

    val maximum_log_likelihood :
      ?debug:bool ->
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      stationary_distribution:Amino_acid.vector ->
      tree ->
      site ->
      float * param

    val simulate_site :
      exchangeability_matrix:Amino_acid.matrix ->
      stationary_distribution:Amino_acid.vector ->
      (_, _, branch_info) Tree.t ->
      param:param ->
      simulation
  end

  module Model2 : sig
    type param = {
      scale : float ;
      stationary_distribution : Amino_acid.vector ;
    }

    val maximum_log_likelihood :
      ?debug:bool ->
      ?mode:[< `dense | `sparse > `sparse ] ->
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      tree ->
      site ->
      float * param

    val lrt :
      ?mode:[< `dense | `sparse > `sparse ] ->
      Wag.t ->
      tree ->
      site ->
      Model1.param * param * likelihood_ratio_test
  end

  module Model3 : sig
    type param = {
      scale : float ;
      stationary_distribution0 : Amino_acid.vector ;
      stationary_distribution1 : Amino_acid.vector ;
    }

    val maximum_log_likelihood :
      ?debug:bool ->
      ?mode:[< `dense | `sparse > `sparse ] ->
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      tree ->
      site ->
      float * param

    val lrt :
      ?mode:[< `dense | `sparse > `sparse ] ->
      Wag.t ->
      tree ->
      site ->
      Model2.param * param * likelihood_ratio_test

    val simulate_site :
      exchangeability_matrix:Rate_matrix.Amino_acid.t ->
      scale:float ->
      stationary_distribution0:Amino_acid.vector ->
      stationary_distribution1:Amino_acid.vector ->
      tree ->
      simulation
  end
end

module type Leaf_info = sig
  type t
  type species
  val species : t -> species
  val condition : t -> [`Ancestral | `Convergent]
end

module type Branch_info = sig
  type t
  val length : t -> float
  val condition : t -> [`Ancestral | `Convergent]
end

module type Site = sig
  type t
  type species
  val get_aa : t -> species -> Amino_acid.t
end

module Make(Branch_info : Branch_info)(Leaf_info : Leaf_info)(Site : Site with type species = Leaf_info.species) = struct
  module Simulator = Simulator.Make(Amino_acid)(Evolution_model)(Branch_info)

  type simulation = (Amino_acid.t, Amino_acid.t, Branch_info.t) Tree.t

  let aa_of_leaf_info site li =
    Site.get_aa site (Leaf_info.species li)

  module Model1 = struct
    type param = float

    let log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale tree site =
      let pi = (stationary_distribution : Amino_acid.vector :> vec) in
      let p = { Evolution_model.scale = 10. ** scale ;
                exchangeability_matrix ;
                stationary_distribution } in
      let transition_matrix =
        let f = Evolution_model.transition_probability_matrix p in
        fun b -> (f (Branch_info.length b) :> mat)
      in
      let leaf_state = aa_of_leaf_info site in
      CTMC.pruning tree ~transition_matrix ~leaf_state ~root_frequencies:pi

    let clip f param =
      if Float.(param.(0) > 3.) then Float.infinity
      else f param

    let decode_vec p = p.(0)

    let inner_maximum_log_likelihood ?debug ~exchangeability_matrix ~stationary_distribution tree site =
      let f vec =
        let scale = decode_vec vec in
        -. log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale tree site
      in
      let sample () = [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |] in
      let ll, p_star = Nelder_mead.minimize ?debug ~tol ~maxit:10_000 ~f:(clip f) ~sample () in
      -. ll, p_star

    let maximum_log_likelihood ?debug ~exchangeability_matrix ~stationary_distribution tree site =
      let ll, vec = inner_maximum_log_likelihood ?debug ~exchangeability_matrix ~stationary_distribution tree site in
      ll, decode_vec vec

    let simulate_site ~exchangeability_matrix ~stationary_distribution tree ~param:scale =
      let root = choose_aa stationary_distribution in
      let p = {
        Evolution_model.stationary_distribution ; scale ;
        exchangeability_matrix ;
      }
      in
      Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p)

  end

  module Model2 = struct
    type param = {
      scale : float ;
      stationary_distribution : Amino_acid.vector ;
    }

    let log_likelihood ~exchangeability_matrix ~param:{ stationary_distribution ; scale } tree site =
      let p = { Evolution_model.scale ; exchangeability_matrix ; stationary_distribution } in
      let transition_matrix =
        let f = Evolution_model.transition_probability_matrix p in
        fun b -> (f (Branch_info.length b) :> mat)
      in
      let leaf_state = aa_of_leaf_info site in
      CTMC.pruning tree ~transition_matrix ~leaf_state ~root_frequencies:(stationary_distribution :> Vector.t)

    let counts xs =
      Amino_acid.Table.init (fun aa -> List.count xs ~f:(Amino_acid.equal aa))

    type vec_schema = {
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

    let decode_vec schema param =
      let stationary_distribution = extract_frequencies ~offset:1 schema param in
      let scale = 10. ** param.(0) in
      { scale ; stationary_distribution }

    let inner_maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site =
      let counts =
        Tree.leaves tree
        |> List.map ~f:(aa_of_leaf_info site)
        |> counts
      in
      let schema = param_schema ?mode counts in
      let theta0 = initial_param schema counts in
      let sample = nelder_mead_init theta0 in
      let f p =
        let param = decode_vec schema p in
        -. log_likelihood ~exchangeability_matrix ~param tree site
      in
      let ll, p_star = Nelder_mead.minimize ~tol ?debug ~maxit:10_000 ~f:(Model1.clip f) ~sample () in
      -. ll, schema, p_star

    let maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site =
      let ll, schema, vec = inner_maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site in
      ll, decode_vec schema vec

    let simulate_profile alpha =
      Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:Amino_acid.card alpha)
      |> Amino_acid.Vector.of_array_exn

    let lrt ?mode (wag : Wag.t) tree site =
      let exchangeability_matrix = wag.rate_matrix in
      let stationary_distribution = wag.freqs in
      let reduced_log_likelihood, p1 =
        Model1.maximum_log_likelihood ~exchangeability_matrix ~stationary_distribution tree site in
      let full_log_likelihood, schema, p2 =
        inner_maximum_log_likelihood ?mode ~exchangeability_matrix tree site in
      let df = float (Array.length p2 - 1 - 1) in
      let lrt = lrt ~full_log_likelihood ~reduced_log_likelihood ~df in
      p1, decode_vec schema p2, lrt
  end

  module Model3 = struct
    type param = {
      scale : float ;
      stationary_distribution0 : Amino_acid.vector ;
      stationary_distribution1 : Amino_acid.vector ;
    }

    let evolution_model_param exchangeability_matrix param cond =
      let f stationary_distribution =
        { Evolution_model.scale = param.scale ; exchangeability_matrix ; stationary_distribution }
      in
      match cond with
      | `Ancestral -> f param.stationary_distribution0
      | `Convergent -> f param.stationary_distribution1
      | _ -> assert false

    let log_likelihood ~exchangeability_matrix ~param tree site =
      let f cond =
        evolution_model_param exchangeability_matrix param cond
        |> Evolution_model.transition_probability_matrix
      in
      let transition_matrix =
        let f0 = f `Ancestral in (* pre-computation *)
        let f1 = f `Convergent in
        fun b ->
          let bl = Branch_info.length b in
          match Branch_info.condition b with
          | `Ancestral -> (f0 bl :> mat)
          | `Convergent -> (f1 bl :> mat)
      in
      let root_frequencies = (param.stationary_distribution0 :> Vector.t) in
      let leaf_state = aa_of_leaf_info site in
      CTMC.pruning tree ~transition_matrix ~leaf_state ~root_frequencies

    let tuple_map (x, y) ~f = (f x, f y)

    let counts tree site =
      Tree.leaves tree
      |> List.partition_tf ~f:(fun l ->
          match Leaf_info.condition l with
          | `Ancestral -> true
          | `Convergent -> false
        )
      |> tuple_map ~f:(List.map ~f:(aa_of_leaf_info site))
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

    let decode_vec schema vec =
      let
        stationary_distribution0,
        stationary_distribution1 = extract_frequencies schema vec in
      let scale = 10. ** vec.(0) in
      { scale ; stationary_distribution0 ; stationary_distribution1 }

    let inner_maximum_log_likelihood ?debug ?mode ?model2_opt ~exchangeability_matrix tree site =
      let schema =
        Tree.leaves tree
        |> List.map ~f:(aa_of_leaf_info site)
        |> Model2.counts
        |> Model2.param_schema ?mode
      in
      let theta0 =
        match model2_opt with
        | None -> initial_param schema tree site
        | Some param -> Array.(append param (sub param ~pos:1 ~len:(length param - 1)))
      in
      let f vec =
        let param = decode_vec schema vec in
        -. log_likelihood ~exchangeability_matrix ~param tree site
      in
      let sample = Model2.nelder_mead_init theta0 in
      let ll, p_star = Nelder_mead.minimize ~tol ?debug ~maxit:10_000 ~f:(Model1.clip f) ~sample () in
      -. ll, schema, p_star

    let maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site =
      let ll, schema, vec = inner_maximum_log_likelihood ?debug ?mode ~exchangeability_matrix tree site in
      ll, decode_vec schema vec

    let lrt ?mode (wag : Wag.t) tree site =
      let exchangeability_matrix = wag.rate_matrix in
      let reduced_log_likelihood, schema2, p2 =
        Model2.inner_maximum_log_likelihood ?mode ~exchangeability_matrix tree site in
      let full_log_likelihood, schema3, p3 =
        inner_maximum_log_likelihood ?mode ~model2_opt:p2 ~exchangeability_matrix tree site in
      let df = float (Array.length p3 - Array.length p2 - 1) in
      let lrt = lrt ~full_log_likelihood ~reduced_log_likelihood ~df in
      Model2.decode_vec schema2 p2,
      decode_vec schema3 p3,
      lrt

    let simulate_site ~exchangeability_matrix ~scale ~stationary_distribution0 ~stationary_distribution1 tree =
      let param b =
        evolution_model_param
          exchangeability_matrix
          { scale ; stationary_distribution0 ; stationary_distribution1 }
          (Branch_info.condition b)
      in
      let root = choose_aa stationary_distribution0 in
      Simulator.site_gillespie_first_reaction tree ~root ~param
  end
end

module Implementation_check = struct
  module Leaf_info = struct
    type species = int
    type t = species * Convsim.condition
    let condition = snd
    let species = fst
  end

  module Site = struct
    type t = Amino_acid.t array
    type species = int
    let get_aa = Array.get
    let of_simulation s =
      Tree.leaves s
      |> Array.of_list
  end

  include Make(Convsim.Branch_info)(Leaf_info)(Site)

  let likelihood_plot_demo (wag : Wag.t) =
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100 in
    let root = choose_aa wag.freqs in
    let true_scale = 1. in
    let p = Evolution_model.param_of_wag wag true_scale in
    let site =
      Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p)
      |> Site.of_simulation
    in
    let ll, scale_hat =
      Model1.maximum_log_likelihood
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        tree site
    in
    let f scale =
      Model1.log_likelihood
        ~exchangeability_matrix:wag.rate_matrix
        ~stationary_distribution:wag.freqs
        ~scale
        tree site
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

  let lrt_1_vs_2_null_simulation ?(seed = 31415926535897931) ?mode (wag : Wag.t) =
    Owl_stats_prng.init seed ;
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:30 in
    let true_scale = 1. in
    let f _ =
      let simulation =
        Model1.simulate_site
          ~exchangeability_matrix:wag.rate_matrix
          ~stationary_distribution:wag.freqs
          ~param:true_scale tree
      in
      let site = Site.of_simulation simulation in
      let p1, p2, lrt = Model2.lrt ?mode wag tree site in
      simulation, p1, p2, lrt
    in
    Array.init 1_000 ~f

  let lrt_2_vs_3_null_simulation ?(seed = 31415926535897931) ?mode ?(alpha = 0.1) ?(nb_simulations = 1_000) (wag : Wag.t) =
    Owl_stats_prng.init seed ;
    let tree = Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:30 in
    let true_scale = 1. in
    let f _ =
      let stationary_distribution = Model2.simulate_profile alpha in
      let simulation =
        Model1.simulate_site
          ~exchangeability_matrix:wag.rate_matrix
          ~stationary_distribution
          ~param:true_scale tree
      in
      let site = Site.of_simulation simulation in
      let p2, p3, lrt = Model3.lrt ?mode wag tree site in
      simulation, p2, p3, lrt
    in
    Array.init nb_simulations ~f
end
