open Core_kernel

module type S = sig
  type vector
  type matrix
  type symbol
  type t = private matrix

  val make : (symbol -> symbol -> float) -> t

  val jc69 : unit -> t

  val gtr :
    equilibrium_frequencies:vector ->
    transition_rates:Owl.Arr.arr -> (* FIXME: introduce symmetric matrices? *)
    t

  val stationary_distribution : t -> vector

  val scaled_rate_matrix : vector -> t -> t

  val scale : t -> t
end

module Make(A : Alphabet.S_int) = struct
  type t = A.matrix
  let sum f =
    List.fold A.all ~init:0. ~f:(fun acc n -> acc +. f n)

  let random_profile alpha =
    Owl.Stats.dirichlet_rvs ~alpha:(Array.create ~len:A.card alpha)
    |> A.Vector.of_array_exn

  let stationary_distribution (m : A.matrix) =
    let module M = Owl.Mat in
    let a = M.(transpose (m :> mat) @= ones 1 A.card)
    and b = M.init_2d (A.card + 1) 1 (fun i _ -> if i < A.card then 0. else 1.) in
    Owl.Linalg.D.linsolve a b
    |> A.vector_of_arr_exn

  let jc69 () =
    let r = Float.(1. / (of_int A.card - 1.)) in
    A.Matrix.init (fun i j -> if A.equal i j then -1. else r)

  let make f =
    let r = A.Matrix.init (fun _ _ -> 0.) in
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
        Owl.Arr.get rates [| ut_index (min i j) (max i j) |] *. stationary_distribution.A.%(jj)
      ) in
    scaled_rate_matrix stationary_distribution m

  let%test "gtr stationary distribution" =
    let pi = random_profile 10. in
    let gtr_params = Utils.random_profile (A.card * (A.card - 1) / 2) in
    let gtr_rates = gtr ~equilibrium_frequencies:pi ~transition_rates:gtr_params in
    let pi' = stationary_distribution gtr_rates in
    Owl.Arr.approx_equal (pi :> Owl.Arr.arr) (pi' :> Owl.Arr.arr)
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
    Nucleotide.Matrix.init (fun i j ->
        if i = j then -1.
        else if Nucleotide.transversion i j then kappa /. (kappa +. 2.)
        else 1. /. (kappa +. 2.)
      )
end
