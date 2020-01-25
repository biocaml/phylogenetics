open Core_kernel

type data = {
  k : int ;
  x1 : int array ;
  x2 : int array ;
}

let data ~x1 ~x2 =
  let k1 = Array.length x1 in
  let k2 = Array.length x2 in
  if k1 <> k2 then raise (Invalid_argument "Multinomial_test.data: the two arrays should have the same size") ;
  { k = k1 ; x1 ; x2 }

type result = {
  _T_ : float ;
  pvalue : float ;
}

let isum = Array.reduce_exn ~f:( + )

let sum a b f =
  let rec loop acc a b =
    if a >= b then acc
    else loop (acc +. f a) (a + 1) b
  in
  loop 0. a b

let%test _ =
  Float.(sum 0 4 (fun i -> [| 1. ; 2. ; 3. ; 4. |].(i)) = 10.)

let frequencies c =
  let n = float @@ Array.reduce_exn ~f:( + ) c in
  Array.map c ~f:(fun c -> float c /. n)

let summed_counts { k ; x1 ; x2 } =
  Array.init k ~f:(fun i -> x1.(i) + x2.(i))

let simulation_test ?(sample_size = 10_000) ~likelihood_log_ratio d =
  let freq = frequencies (summed_counts d) in
  let t_obs = likelihood_log_ratio d in
  let c = ref 0 in
  let n1 = isum d.x1 in
  let n2 = isum d.x2 in
  for _ = 1 to sample_size do
    let d = {
      k = d.k ;
      x1 = Owl.Stats.multinomial_rvs ~p:freq n1 ;
      x2 = Owl.Stats.multinomial_rvs ~p:freq n2 ;
    }
    in
    let _T_ = likelihood_log_ratio d in
    if Float.(_T_ >= t_obs) then incr c
  done ;
  let pvalue = float !c /. float sample_size in
  { _T_ = t_obs ; pvalue }

module LRT = struct
  let likelihood_log_ratio ({ x1 ; x2 ; _ } as d) =
    let logpdf = Owl.Stats.multinomial_logpdf in
    let log_prod0 =
      let p = frequencies (summed_counts d) in
      logpdf x1 ~p +. logpdf x2 ~p
    in
    let log_prod1 =
      logpdf x1 ~p:(frequencies x1) +. logpdf x2 ~p:(frequencies x2)
    in
    -2. *. (log_prod0 -. log_prod1)

  let asymptotic_test d =
    let _T_ = likelihood_log_ratio d in
    let pvalue = 1. -. Owl.Stats.chi2_cdf _T_ ~df:(float (d.k - 1)) in
    { _T_ ; pvalue }

  let simulation_test = simulation_test ~likelihood_log_ratio
end

(**
   Implementation of

   Two-Sample Test for Sparse High Dimensional Multinomial Distributions
   Amanda Plunkett and Junyong Park
*)
module Sparse = struct
  let f_star ~n1 ~n2 ~x1 ~x2 =
    (x1 /. n1 -. x2 /. n2) ** 2. -. x1 /. (n1 ** 2.) -. x2 /. (n2 ** 2.)

  let likelihood_log_ratio { k ; x1 ; x2 } =
    let n1 = float (isum x1) in
    let n2 = float (isum x2) in
    let p1_hat = Array.map x1 ~f:(fun x -> float x /. n1) in
    let p2_hat = Array.map x2 ~f:(fun x -> float x /. n2) in
    let sigma_k_hat_squared =
      let f n p_hat =
        sum 0 k (fun i -> 2. /. (n ** 2.) *. (p_hat.(i) ** 2. -. p_hat.(i) /. n))
      in
      f n1 p1_hat +. f n2 p2_hat +. 4. /. n1 /. n2 *. sum 0 k (fun i -> p1_hat.(i) *. p2_hat.(i))
    in
    let sigma_k_hat = sqrt sigma_k_hat_squared in
    sum 0 k (fun i -> f_star ~n1 ~n2 ~x1:(float x1.(i)) ~x2:(float x2.(i)))
    /. sigma_k_hat

  let asymptotic_test d =
    let _T_ = likelihood_log_ratio d in
    let pvalue = 1. -. Owl.Stats.gaussian_cdf _T_ ~mu:0. ~sigma:1. in
    { _T_ ; pvalue }

  let simulation_test = simulation_test ~likelihood_log_ratio
end

let random_discrete_probability k ~alpha =
  let x = Array.init k ~f:(fun _ -> Owl.Stats.gamma_rvs ~shape:alpha ~scale:1.) in
  let s = Array.reduce_exn x ~f:( +. ) in
  Array.map x ~f:(fun x -> x /. s)

let counts n p =
  let r = Array.map p ~f:(fun _ -> 0) in
  for _ = 1 to n do
    let i = Owl.Stats.categorical_rvs p in
    r.(i) <- r.(i) + 1
  done ;
  r

let h0_sample ~n1 ~n2 p =
  counts n1 p, counts n2 p

let uniformity_test ~k ~n1 ~n2 test =
  let p_values =
    Array.init 1_000 ~f:(fun _ ->
        let p = random_discrete_probability k ~alpha:10. in
        let x1, x2 = h0_sample ~n1 ~n2 p in
        (test { k ; x1 ; x2 }).pvalue
      )
  in
  ignore (OCamlR_graphics.hist ~breaks:(`n 20) p_values : OCamlR_graphics.hist)

let example = {
  k = 20 ;
  x1 = [| 5 ; 3 ; 1 ; 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 |] ;
  x2 = [| 0 ; 0 ; 0 ; 0 ; 7 ; 2 ; 1 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 ; 0 |]
}
