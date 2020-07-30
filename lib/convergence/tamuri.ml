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

let model1_maximum_likelihood ~exchangeability_matrix ~stationary_distribution site =
  let module CTMC = Phylo_ctmc.Make(Amino_acid) in
  let pi = (stationary_distribution : Amino_acid.vector :> vec) in
  let f param =
    let p = { Evolution_model.scale = 10. ** param.(0) ;
              exchangeability_matrix ;
              stationary_distribution } in
    let transition_matrix =
      let f = Evolution_model.transition_probability_matrix p in
      fun (bl, _) -> (f bl :> mat)
    in
    -. CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:pi
  in
  let sample () = [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |] in
  let ll, p_star = Nelder_mead.minimize ~debug:true ~maxit:100 ~f ~sample () in
  ll, p_star.(0)

let model1_likelihood ~exchangeability_matrix ~stationary_distribution site value =
  let module CTMC = Phylo_ctmc.Make(Amino_acid) in
  let pi = (stationary_distribution : Amino_acid.vector :> vec) in
  let f param =
    let p = { Evolution_model.scale = 10. ** param.(0) ;
              exchangeability_matrix ;
              stationary_distribution } in
    let transition_matrix =
      let f = Evolution_model.transition_probability_matrix p in
      fun (bl, _) -> (f bl :> mat)
    in
    -. CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:pi
  in
  f value

let model1_demo (wag : Wag.t) =
  let tree =
    Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:6
    |> Tree.map ~node:Fn.id ~leaf:Fn.id ~branch:(fun bi ->
        Option.value_exn bi.length,
        List.Assoc.find_exn ~equal:String.equal bi.tags "Condition"
      )
  in
  let root =
    wag.freqs
    |> Amino_acid.Table.of_vector
    |> Amino_acid.Table.choose
  in
  let true_scale = 1. in
  let p = Evolution_model.param_of_wag wag true_scale in
  let site = Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p) in
  let ll, scale_hat =
    model1_maximum_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      ~stationary_distribution:wag.freqs
      site
  in
  let f x =
    model1_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      ~stationary_distribution:wag.freqs
      site
      [|x|]
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

let sigmoid x = 1. /. (1. +. Float.exp (-. x))
let logit p = Float.log (p /. (1. -. p))

let model2_log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site =
  let module CTMC = Phylo_ctmc.Make(Amino_acid) in
  let p = { Evolution_model.scale ; exchangeability_matrix ; stationary_distribution } in
  let transition_matrix =
    let f = Evolution_model.transition_probability_matrix p in
    fun (bl, _) -> (f bl :> mat)
  in
  CTMC.pruning site ~transition_matrix ~leaf_state:Fn.id ~root_frequencies:(stationary_distribution :> Vector.t)

let counts xs =
  Amino_acid.Table.init (fun aa -> List.count xs ~f:(Amino_acid.equal aa))

let leaves_counts site =
  let counts = (counts (Tree.leaves site) : int Amino_acid.table :> _ array) in
  Array.map counts ~f:float

type model2_param_schema = {
  counts : int Amino_acid.table ;
  nz : int ; (* number of non-zero AA in count table *)
  idx : int array ; (* indices of non-zero AA *)
}

let model2_param_schema site =
  let counts = counts (Tree.leaves site) in
  let k = (counts : int Amino_acid.table :> _ array) in
  let idx, nz = Array.foldi k ~init:([], 0) ~f:(fun i ((assoc, nz) as acc) k_i ->
      if k_i = 0 then acc else i :: assoc, nz + 1
    )
  in
  let idx = Array.of_list idx in
  { nz ; idx ; counts }

let initial_param schema =
  let counts = (schema.counts : int Amino_acid.table :> _ array) in
  let total_counts = Array.fold counts ~init:0. ~f:(fun acc x -> acc +. float x) in
  let pi_0 = Array.map schema.idx ~f:(fun idx -> Float.log (float counts.(idx) /. total_counts)) in
  Array.append [| 0. |] pi_0

let extract_frequencies schema param =
  let r = Array.create ~len:Amino_acid.card 0. in
  Array.iteri schema.idx ~f:(fun sparse_idx full_idx ->
      r.(full_idx) <- Float.exp param.(sparse_idx + 1)
    ) ;
  let s = Owl.Stats.sum r in
  Amino_acid.Vector.init (fun aa -> r.((aa :> int)) /. s)

let model2_maximum_likelihood ~exchangeability_matrix site =
  let schema = model2_param_schema site in
  let theta0 = initial_param schema in
  let f param =
    let stationary_distribution = extract_frequencies schema param in
    let scale = 10. ** param.(0) in
    -. model2_log_likelihood ~exchangeability_matrix ~stationary_distribution ~scale site
  in
  let sample =
    let c = ref (-1) in
    fun _ ->
      incr c ;
      if !c = 0 then theta0
      else
        Array.init (Array.length theta0) ~f:(fun i ->
            theta0.(i) +. if i = !c - 1 then  -. 1. else 0.
          )
  in
  printf "%f\n" (f theta0) ;
  let ll, p_star = Nelder_mead.minimize ~debug:true ~maxit:100 ~f ~sample () in
  ll, p_star

let scale xs =
  let sum = Array.fold xs ~init:0. ~f:( +. ) in
  Array.map xs ~f:(fun x -> x /. sum)

let demo_site (wag : Wag.t) =
  let tree =
    Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100
    |> Tree.map ~node:Fn.id ~leaf:Fn.id ~branch:(fun bi ->
        Option.value_exn bi.length,
        List.Assoc.find_exn ~equal:String.equal bi.tags "Condition"
      )
  in
  let true_pi =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:Amino_acid.card 0.1)
    |> Amino_acid.Vector.of_array_exn
  in
  let root =
    true_pi
    |> Amino_acid.Table.of_vector
    |> Amino_acid.Table.choose
  in
  let true_scale = 1. in
  let p = {
    Evolution_model.stationary_distribution = true_pi ; scale = true_scale ;
    exchangeability_matrix = wag.rate_matrix
  }
  in
  let site = Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p) in
  site

let model2_demo (wag : Wag.t) =
  let tree =
    Convsim.pair_tree ~branch_length1:1. ~branch_length2:1. ~npairs:100
    |> Tree.map ~node:Fn.id ~leaf:Fn.id ~branch:(fun bi ->
        Option.value_exn bi.length,
        List.Assoc.find_exn ~equal:String.equal bi.tags "Condition"
      )
  in
  let true_pi =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:Amino_acid.card 0.1)
    |> Amino_acid.Vector.of_array_exn
  in
  let root =
    true_pi
    |> Amino_acid.Table.of_vector
    |> Amino_acid.Table.choose
  in
  let true_scale = 1. in
  let p = {
    Evolution_model.stationary_distribution = true_pi ; scale = true_scale ;
    exchangeability_matrix = wag.rate_matrix
  }
  in
  let site = Simulator.site_gillespie_first_reaction tree ~root ~param:(Fn.const p) in
  let ll_star = model2_log_likelihood ~exchangeability_matrix:wag.rate_matrix ~stationary_distribution:true_pi ~scale:true_scale site in
  let pi_0 = (leaves_counts site) in
  let show x = Array.to_list x |> List.map ~f:(sprintf "%.2f") |> String.concat ~sep:" " in
  print_endline (show (scale pi_0)) ;
  printf "LL* = %g\n" ll_star ;
  let ll, p_hat =
    model2_maximum_likelihood
      ~exchangeability_matrix:wag.rate_matrix
      site
  in
  printf "LL = %g, scale_hat = %g\n" ll p_hat.(0)

let leaves_freqs site =
  let counts = (counts (Tree.leaves site) : int Amino_acid.table :> _ array) in
  let n = float @@ Array.fold counts ~init:0 ~f:( + ) in
  Array.map counts ~f:(fun k -> logit (float k /. n))

let model3_maximum_likelihood ~exchangeability_matrix site =
  let pi_0 = leaves_freqs site in
  let f param =
    let pi =
      let v = Amino_acid.Vector.init (fun aa -> sigmoid param.((aa :> int) + 1)) in
      let s = Amino_acid.Vector.sum v in
      Amino_acid.Vector.map v ~f:(fun x -> x /. s)
    in
    -. model2_log_likelihood ~exchangeability_matrix ~stationary_distribution:pi ~scale:(10. ** param.(0)) site
  in
  let sample =
    let c = ref (-1) in
    fun _ ->
      incr c ;
      if !c = 0 then
        Array.append
          [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |]
          pi_0
      else
        Array.append
          [| Owl.Stats.uniform_rvs ~a:(-4.) ~b:1. |]
          (Array.init Amino_acid.card ~f:(fun i -> if i = !c - 1 then 3. else -3.))
  in
  let ll, p_star = Nelder_mead.minimize ~debug:true ~maxit:10_000 ~f ~sample () in
  ll, p_star
