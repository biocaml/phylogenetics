open Core
open Linear_algebra

module type S = sig
  type vector
  type matrix
  type symbol
  type t = matrix

  val make : (symbol -> symbol -> float) -> t

  val jc69 : unit -> t

  val gtr :
    equilibrium_frequencies:vector ->
    transition_rates:vec -> (* FIXME: introduce symmetric matrices? *)
    t

  val stationary_distribution : t -> vector

  val scaled_rate_matrix : vector -> t -> t

  val scale : t -> t
end

module Make(A : Alphabet.S_int) = struct
  type t = A.matrix
  let sum f =
    List.fold A.all ~init:0. ~f:(fun acc n -> acc +. f n)

  let stationary_distribution (m : A.matrix) =
    Matrix.zero_eigen_vector (m :> mat)
    |> A.Vector.upcast_exn

  let jc69 () =
    let r = Float.(1. / (of_int A.card - 1.)) in
    A.Matrix.init (fun i j -> if A.equal i j then -1. else r)

  let make f =
    let r = A.Matrix.init (fun _ _ -> 0.) in
    List.iter A.all ~f:(fun i->
        let total = ref 0. in
        List.iter A.all ~f:(fun j ->
            if not (A.equal i j) then (
              let r_ij = f i j in
              if Float.(r_ij < 0.) then (failwith "Rates should be positive") ;
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

  let scale rate =
    let mu =
      sum (fun i ->
          sum (fun j ->
              if A.equal i j then 0. else rate.A.%{i, j}
            )
        )
    in
    A.Matrix.init Float.(fun i j -> rate.A.%{i, j} / mu)

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

  let gtr ~equilibrium_frequencies:(stationary_distribution : A.vector)
      ~transition_rates:rates =
    let m = make (fun i j ->
        let jj = j in
        let i = (i :> int) in
        let j = (j :> int) in
        Vector.get rates (ut_index (min i j) (max i j)) *. stationary_distribution.A.%(jj)
      ) in
    scaled_rate_matrix stationary_distribution m

  let%test "gtr stationary distribution" =
    let rng = Utils.rng_of_int 12334 in
    let pi = A.random_profile rng 10. in
    let gtr_params = Utils.random_profile rng (A.card * (A.card - 1) / 2) in
    let gtr_rates = gtr ~equilibrium_frequencies:pi ~transition_rates:gtr_params in
    let pi' = stationary_distribution gtr_rates in
    Vector.robust_equal ~tol:1e-6 (pi :> vec) (pi' :> vec)
end

let make n ~f =
  let r = Matrix.init n ~f:(fun _ _ -> 0.) in
  for i = 0 to n - 1 do
    let total = ref 0. in
    for j = 0 to n - 1 do
      if i <> j then (
        let r_ij = f i j in
        if Float.(r_ij < 0.) then (failwith "Rates should be positive") ;
        total := r_ij +. !total ;
        Matrix.set r i j r_ij
      )
    done ;
    Matrix.set r i i (-. !total)
  done ;
  r

let transition_probability_matrix ~tau ~rates =
  Matrix.((
      (of_arrays_exn rates
       |> scal_mul tau
       |> expm) :> Lacaml.D.mat)
    )
  |> Lacaml.D.Mat.to_array

module Nucleotide = struct
  include Make(Nucleotide)

  let k80 kappa =
    Nucleotide.Matrix.init (fun i j ->
        if Nucleotide.equal i j then -1.
        else if Nucleotide.transversion i j then 1. /. (kappa +. 2.)
        else kappa /. (kappa +. 2.)
      )

  let hky85
      ~equilibrium_frequencies:(stationary_distribution : Nucleotide.vector)
      ~transition_rate ~transversion_rate =
    let m = make (fun i j ->
        let coef = if Nucleotide.equal i j
          then -1.
          else if Nucleotide.transversion i j
          then transversion_rate
          else transition_rate
        in
        coef *. stationary_distribution.Nucleotide.%(j)
      ) in
    scaled_rate_matrix stationary_distribution m

  let%test "HKY85 stationary distribution" =
    let rng = Utils.rng_of_int 420 in
    let pi = Nucleotide.random_profile rng 10. in
    let transition_rate = Gsl.Rng.uniform rng
    and transversion_rate = Gsl.Rng.uniform rng in
    let hky_rates = hky85 ~equilibrium_frequencies:pi ~transition_rate ~transversion_rate
    in
    let pi' = stationary_distribution hky_rates in
    Vector.robust_equal ~tol:1e-6 (pi :> vec) (pi' :> vec)
end

module Amino_acid = struct
  include Make(Amino_acid)
end
