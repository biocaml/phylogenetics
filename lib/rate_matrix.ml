open Core_kernel

module type S = sig
  type 'a vector
  type 'a matrix
  type symbol
  type t = private float matrix

  val make : (symbol -> symbol -> float) -> t

  val jc69 : unit -> t

  val gtr :
    equilibrium_frequencies:float vector ->
    transition_rates:float array -> (* FIXME: introduce symmetric matrices? *)
    t

  val stationary_distribution : t -> float vector

  val scaled_rate_matrix : float vector -> t -> t

  val scale_matrix : float matrix -> float matrix
end

module Make(A : Alphabet.S_int) = struct
  type t = float A.matrix
  let sum f =
    List.fold A.all ~init:0. ~f:(fun acc n -> acc +. f n)

  let random_profile alpha =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:A.card alpha)
    |> A.vector_of_array_exn

  let stationary_distribution (m : float A.matrix) =
    let module M = Owl.Mat in
    let a =
      Array.(
        append
          (transpose_exn (m :> float array array))
          [| Array.create ~len:A.card 1. |]
      )
      |> M.of_arrays
    and b = M.init_2d (A.card + 1) 1 (fun i _ -> if i < A.card then 0. else 1.) in
    Owl.Linalg.D.linsolve a b
    |> Owl.Mat.to_array
    |> A.vector_of_array_exn

  let jc69 () =
    let r = Float.(1. / (of_int A.card - 1.)) in
    A.matrix (fun i j -> if A.equal i j then -1. else r)

  let make f =
    let r = A.matrix (fun _ _ -> 0.) in
    List.iter A.all ~f:(fun i->
      let total = ref 0. in
      List.iter A.all ~f:(fun j ->
          if i <> j then (
            let r_ij = f i j in
            if r_ij < 0. then (failwith "Rates should be positive") ;
            total := r_ij +. !total ;
            A.(r.%{i, j} <- r_ij)
          )
        ) ;
      A.(r.%{i, i} <- -. !total)
      ) ;
    r

  let scaled_rate_matrix profile rate =
    let mu = -. sum Float.(fun i ->
        profile.A.%(i) * rate.A.%{i, i}
      )
    in
    make Float.(fun i j -> rate.A.%{i, j} / mu)

  let scale_matrix rate =
    let mu =
      sum (fun i ->
          sum (fun j ->
              if A.equal i j then 0. else rate.A.%{i, j}
            )
        )
    in
    A.matrix Float.(fun i j -> rate.A.%{i, j} / mu)

  let ut_index i j =
    let n = A.card in
    n * (n - 1) / 2 - (n - i) * (n - i -1) / 2 + j - i - 1

  let%test "upper triangular indexation" =
    Poly.equal
      (
        List.init A.card ~f:(fun i ->
            List.init (A.card - i - 1) ~f:(fun j ->
                let j = i + j + 1 in
                ut_index i j
              )
          )
        |> List.concat
      )
      (List.init (A.card * (A.card - 1) / 2) ~f:Fn.id)

  let gtr ~equilibrium_frequencies:(stationary_distribution : float A.vector)
      ~transition_rates:rates =
    let m = make (fun i j ->
        let jj = j in
        let i = (i :> int) in
        let j = (j :> int) in
        rates.(ut_index (min i j) (max i j)) *. stationary_distribution.A.%(jj)
      ) in
    scaled_rate_matrix stationary_distribution m

  let%test "gtr stationary distribution" =
    let pi = random_profile 10. in
    let gtr_params = Utils.random_profile (A.card * (A.card - 1) / 2) in
    let gtr_rates = gtr ~equilibrium_frequencies:pi ~transition_rates:gtr_params in
    let pi' = stationary_distribution gtr_rates in
    Utils.float_array_robust_equal (pi :> float array) (pi' :> float array)
end


let transition_probability_matrix ~tau ~rates =
  Owl.Mat.(
    of_arrays rates *$ tau
    |> Owl.Linalg.D.expm
    |> to_arrays
  )

module Nucleotide = struct
  include Make(Nucleotide)

  let k80 kappa =
    Nucleotide.matrix (fun i j ->
        if i = j then -1.
        else if Nucleotide.transversion i j then kappa /. (kappa +. 2.)
        else 1. /. (kappa +. 2.)
      )
end
