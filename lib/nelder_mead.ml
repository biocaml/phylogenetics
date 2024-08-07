(**
   Implements method as described in https://en.wikipedia.org/wiki/Nelder%E2%80%93Mead_method
   some tests from original publication:

     A simplex method for function minimization
     J. A. Nelder and R. Mead


  Here is what the different functions of this file do:

  - `centroid` calculates the centroid of a set of points. In the
  context of the Nelder-Mead method, it is used to compute the center
  of gravity of a simplex, which is a polytope composed of points in a
  multidimensional space.

  - update performs an update of a point using a specified
  direction. This can be used to update a point in the Nelder-Mead
  method according to the specific rules of the algorithm.

  - `minimize` is the main function that implements the Nelder-Mead
  method to minimize a given function.  It takes parameters such as
  the function to minimize (f), a sample generation function (sample)
  to generate initial points, as well as optional parameters like
  tolerance (tol), maximum number of iterations (maxit), and debug
  mode (debug).


  Using these functions, you can perform the minimization of a given
  function using the Nelder-Mead method. The algorithm iterates over a
  set of points (a simplex) and performs updates to get closer to the
  minimum of the function. The process repeats until a convergence
  condition is met (e.g., when the difference between function values
  becomes smaller than a given tolerance). The minimize function
  returns the obtained minimum, the corresponding point, and the
  number of iterations performed.
*)

open Core

let centroid xs =
  let n = Array.length xs in
  if n = 0 then raise (Invalid_argument "Nelder_mead.centroid: empty array") ;
  let d = Array.length xs.(0) in
  Array.init d ~f:(fun i ->
      Array.fold xs ~init:0. ~f:(fun acc x -> acc +. x.(i))
      /. float n
    )

let update ~from:c alpha ~towards:x =
  let d = Array.length c in
  Array.init d ~f:(fun i -> c.(i) +. alpha *. (x.(i) -. c.(i)))

let minimize ?(tol = 1e-8) ?(maxit = 100_000) ?(debug = false) ~f ~sample () =
  let alpha = 1. in
  let gamma = 2. in
  let rho = 0.5 in
  let sigma = 0.5 in
  let x0 = sample () in
  let n = Array.length x0 in
  if n = 0 then raise (Invalid_argument "Nelder_mead.minimize: sample returns empty vectors") ;
  let sample () =
    let y = sample () in
    if Array.length y <> n then raise (Invalid_argument "Nelder_mead.minimize: sample returns vectors of varying lengths") ;
    y
  in
  let points = Array.init (n + 1) ~f:(fun _ -> sample ()) in
  let obj = Array.map points ~f in
  let rec loop i =
    let ranks = Utils.array_order ~compare:Float.compare obj in
    if debug then (
      printf "\n\nIteration %d: %f\n%!" i obj.(ranks.(0)) ;
      printf "Delta: %g\n%!" (obj.(ranks.(n)) -. obj.(ranks.(0)))
    ) ;
    let c =
      Array.sub ranks ~pos:0 ~len:n
      |> Array.map ~f:(Array.get points)
      |> centroid
    in
    let x_r = update ~from:c (-. alpha) ~towards:points.(ranks.(n)) in
    let f_r = f x_r in
    if debug then (
      printf "Candidate: %f\n" f_r ;
    ) ;
    (
      match Float.(f_r < obj.(ranks.(0)), f_r < obj.(ranks.(Int.(n - 1)))) with
      | false, true ->
        if debug then printf "Reflection\n" ;
        points.(ranks.(n)) <- x_r ;
        obj.(ranks.(n)) <- f_r ;
      | true, _ ->
        if debug then printf "Expansion\n" ;
        let x_e = update ~from:c gamma ~towards:x_r in
        let f_e = f x_e in
        points.(ranks.(n)) <- if Float.(f_e < f_r) then x_e else x_r ;
        obj.(ranks.(n)) <- Float.min f_r f_e ;
      | false, false ->
        let x_c, f_c, candidate_accepted =
          if Float.(f_r < obj.(ranks.(n))) then (* outside contraction *)
            let x_c = update ~from:c rho ~towards:x_r in
            let f_c = f x_c in
            x_c, f_c, Float.(f_c <= f_r)
          else (* inside contraction *)
            let x_cc = update ~from:c ~towards:points.(ranks.(n)) rho in
            let f_cc = f x_cc in
            x_cc, f_cc, Float.(f_cc < obj.(ranks.(n)))
        in
        if candidate_accepted then (
          if debug then printf "Contraction, f_c = %f\n" f_c ;
          points.(ranks.(n)) <- x_c ;
          obj.(ranks.(n)) <- f_c ;
        )
        else (
          if debug then printf "Shrink\n" ;
          Array.iteri points ~f:(fun i x_i ->
              if i <> ranks.(0) then (
                let x_i = update ~from:points.(ranks.(0)) sigma ~towards:x_i in
                points.(i) <- x_i ;
                obj.(i) <- f x_i
              )
            )
        )
    ) ;
    let sigma = Gsl.Stats.sd obj in
    if debug then (
      printf "Sigma: %f\n" sigma ;
      printf "Values: %s\n" (Utils.show_float_array (Array.init (n + 1) ~f:(fun i -> obj.(ranks.(i)))))
    ) ;
    if Float.(sigma < tol) || i >= maxit
    then obj.(ranks.(0)), points.(ranks.(0)), i
    else loop (i + 1)
  in
  loop 0

let%test "Parabola" =
  let f x = x.(0) ** 2. in
  let sample () = [| Random.float 200. -. 100. |] in
  let obj, _, _ = minimize ~f ~tol:1e-3 ~sample () in
  Float.(abs obj < 1e-3)

let%test "Rosenbrock" =
  let f x = 100. *. (x.(1) -. x.(0) ** 2.) ** 2. +. (1. -. x.(0)) ** 2. in
  let rfloat _ = Random.float 200. -. 100. in
  let sample () = Array.init 2 ~f:rfloat in
  let obj, _, _ = minimize ~f ~sample () in
  Float.(abs obj < 1e-3)

let%test "Powell quartic" =
  let f x =
    let open Float in
    (x.(0) + 10. * x.(1)) ** 2. + 5. *. (x.(2) - x.(3)) ** 2.
    + (x.(1) - 2. *. x.(2)) ** 4. + 10. * (x.(0) - x.(3)) ** 4.
  in
  let rfloat _ = Random.float 200. -. 100. in
  let sample () = Array.init 4 ~f:rfloat in
  let obj, _, _ = minimize ~f ~sample () in
  Float.(abs obj < 1e-3)
