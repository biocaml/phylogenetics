module type S = sig
  type param
  type vec
  type mat
  val rate_matrix : param -> mat
  val transition_probability_matrix : param -> float -> mat
  val stationary_distribution : param -> vec
end

module type S_with_reduction = sig
  include S
  val rate_matrix_reduction : param -> mat * vec * mat
end

module type Rate_matrix = sig
  type param
  type mat
  val rate_matrix : param -> mat
end

module type Diagonalizable_rate_matrix = sig
  include Rate_matrix
  type vec
  val rate_matrix_reduction : param -> mat * vec * mat
end

module Make
    (A : Alphabet.S)
    (M : Rate_matrix with type mat := A.matrix) =
struct
  let transition_probability_matrix param t =
    A.Matrix.(expm (scal_mul t (M.rate_matrix param)))
  let stationary_distribution param =
    A.Matrix.zero_eigen_vector (M.rate_matrix param)
end


module Make_diag
    (A : Alphabet.S)
    (M : Diagonalizable_rate_matrix with type vec := A.vector
                                     and type mat := A.matrix)
= struct
  let transition_probability_matrix param =
    let diag_p, diag, diag_p_inv = M.rate_matrix_reduction param in
    fun t ->
      let d = A.Matrix.diagm A.Vector.(exp (scal_mul t diag)) in
      A.Matrix.(dot (dot diag_p d) diag_p_inv)

  let stationary_distribution param =
    A.Matrix.zero_eigen_vector (M.rate_matrix param)
end

module JC69 = struct
  module Base = struct
    type param = unit
    let rate_matrix () = Rate_matrix.Nucleotide.jc69 ()
    let rate_matrix_reduction () =
      let p = Nucleotide.Matrix.of_arrays_exn [|
          [| 1. ; -1. ; -1. ; -1. |] ;
          [| 1. ;  1. ;  0. ;  0. |] ;
          [| 1. ;  0. ;  1. ;  0. |] ;
          [| 1. ;  0. ;  0. ;  1. |] ;
        |]
      in
      let diag = Nucleotide.Vector.of_array_exn [| 0. ; -4./.3. ; -4./.3. ; -4./.3. |] in
      let p_inv = Nucleotide.Matrix.of_arrays_exn [|
          [|  0.25 ;  0.25 ;  0.25 ;  0.25 |] ;
          [| -0.25 ;  0.75 ; -0.25 ; -0.25 |] ;
          [| -0.25 ; -0.25 ;  0.75 ; -0.25 |] ;
          [| -0.25 ; -0.25 ; -0.25 ;  0.75 |] ;
        |]
      in
      p, diag, p_inv

    let%test "JC69_reduction 1" =
      let p, _, p_inv = rate_matrix_reduction () in
      Nucleotide.Matrix.(compare ~tol:1e-6 (dot p p_inv) (init (fun i j -> if i = j then 1. else 0.)))

    let%test "JC69_reduction 2" =
      let p, d, p_inv = rate_matrix_reduction () in
      Nucleotide.Matrix.(compare ~tol:1e-6 (dot p (dot (diagm d) p_inv)) (rate_matrix ()))
  end
  include Base
  include Make_diag(Nucleotide)(Base)
  let stationary_distribution () = Nucleotide.Vector.init (fun _ -> 0.25)
end

module JC69_numerical = struct
  include JC69.Base
  include Make(Nucleotide)(JC69.Base)
end

module K80 = struct
  module Base = struct
    type param = float
    let rate_matrix kappa = Rate_matrix.Nucleotide.k80 kappa
    let rate_matrix_reduction k =
      let p = Nucleotide.Matrix.of_arrays_exn [|
          [| 1. ; -1. ;  0. ; -1. |] ;
          [| 1. ;  1. ; -1. ;  0. |] ;
          [| 1. ; -1. ;  0. ;  1. |] ;
          [| 1. ;  1. ;  1. ;  0. |] ;
        |]
      in
      let diag =
        let lambda_3 =  (-2. *. k -. 2.) /. (k +. 2.) in
        [| 0. ; -4. /. (k +. 2.) ; lambda_3 ; lambda_3 |]
        |> Nucleotide.Vector.of_array_exn
      in
      let p_inv = Nucleotide.Matrix.of_arrays_exn [|
          [|  0.25 ;  0.25 ;  0.25 ; 0.25 |] ;
          [| -0.25 ;  0.25 ; -0.25 ; 0.25 |] ;
          [|  0.   ; -0.5  ;  0.   ; 0.5  |] ;
          [| -0.5  ;  0.   ;  0.5  ; 0.   |]
        |]
      in
      p, diag, p_inv

    let%test "K80_reduction 1" =
      let p, _, p_inv = rate_matrix_reduction 2. in
      Nucleotide.Matrix.(compare ~tol:1e-6 (dot p p_inv) (init (fun i j -> if i = j then 1. else 0.)))

    let%test "K80_reduction 2" =
      let p, d, p_inv = rate_matrix_reduction 2. in
      Nucleotide.Matrix.(compare ~tol:1e-6 (dot p (dot (diagm d) p_inv)) (rate_matrix 2.))
  end
  include Base
  include Make_diag(Nucleotide)(Base)
  let stationary_distribution _ = Nucleotide.Vector.init (fun _ -> 0.25)
end

module K80_numerical = struct
  include K80.Base
  include Make(Nucleotide)(K80.Base)
end

module type Nucleotide_S_with_reduction =
  S_with_reduction
  with type vec := Nucleotide.vector
   and type mat := Nucleotide.matrix
