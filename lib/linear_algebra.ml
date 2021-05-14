(** A vector of floats. *)
module type Vector = sig
  type t

  val length : t -> int

  (** Initialises a vector from a int->float function. *)
  val init : int -> f:(int -> float) -> t

  val map : t -> f:(float -> float) -> t

  (** Scalar-vector product (in-place). *)
  val inplace_scal_mul: float -> t -> unit

  (** Scalar-vector product *)
  val scal_mul : float -> t -> t

  (** Scalar-vector addition. *)
  val scal_add: float -> t -> t

  (** Vector addition. *)
  val add : t -> t -> t

  (** Element-wise product of two vectors. *)
  val mul : t -> t -> t

  (** Sum of the elements of a vector. *)
  val sum : t -> float

  (** Element-wise logarithm of vector *)
  val log : t -> t

  (** Element-wise exponential of matrix*)
  val exp : t -> t

  (** Minimum element in a vector. *)
  val min : t -> float

  (** Maximum element in a vector. *)
  val max : t -> float

  (** Access a specific element of a vector. *)
  val get : t -> int -> float

  (** Set a specific element of a vector. *)
  val set : t -> int -> float -> unit

  val robust_equal : tol:float -> t -> t -> bool

  val of_array : float array -> t
  val to_array : t -> float array

  (** Prints a vector to the standard output. *)
  val pp : Format.formatter -> t -> unit
end

(** A square matrix of floats. *)
module type Matrix = sig
  type vec
  type t

  val dim : t -> int * int

  (** {5 Matrix and vector creation} *)

  (** Initialises a square matrix from a int->int->float function. *)
  val init : int -> f:(int -> int -> float) -> t

  (** [init_sym n ~f] creates a symetric square matrix by calling [f]
     only for elements s.t. [i <= j] *)
  val init_sym : int -> f:(int -> int -> float) -> t

  (** Initializes a square diagonal matrix from the vector of its diagonal elements. *)
  val diagm : vec -> t

  (** Matrix element-wise multiplication *)
  val mul : t -> t -> t

  (** Matrix addition. *)
  val add : t -> t -> t

  (** Multiplication of a matrix by a scalar. *)
  val scal_mul : float -> t -> t

  (** Inplace multiplication of a matrix by a scalar. *)
  val inplace_scal_mul: float -> t -> unit

  (** Matrix multiplication *)
  val dot : t -> t -> t

  (** Matrix-vector product *)
  val apply : t -> vec -> vec

  (** Matrix exponentiation *)
  val pow : t -> int -> t

  (** Matrix exponential *)
  val expm : t -> t

  (** Element-wise logarithm of matrix *)
  val log : t -> t

  (** Compares two matrices and tolerates a certain relative difference.
      Let f be the float parameter, it returns true iff the elements of the second matrix
      are between 1-f and 1+f times the corresponding elements of the first *)
  val robust_equal : tol:float -> t -> t -> bool

  (** Access a specific element of a matrix. *)
  val get : t -> int -> int -> float

  (** Set a specific element of a matrix. *)
  val set : t -> int -> int -> float -> unit

  (** Copy row from a matrix *)
  val row : t -> int -> vec

  (** Diagonalizes a matrix M so that M = PxDxP^T; returns (v,P) where
      v is the diagonal vector of D.*)
  val diagonalize : t -> vec * t

  val transpose : t -> t

  (** Computes the inverse of a matrix. *)
  val inverse: t -> t

  (** [zero_eigen_vector m] is a vector [v] such that [Vec.sum v = 1]
      and [mat_vec_mul m v = zero] *)
  val zero_eigen_vector : t -> vec

  val of_arrays : float array array -> t option
  val of_arrays_exn : float array array -> t

  (** Prints a matrix to the standard output (display may be messy). *)
  val pp : Format.formatter -> t -> unit
end

module type S = sig
  type vec
  type mat

  module Vector : Vector with type t = vec
  module Matrix : Matrix with type t = mat and type vec := vec
end

module Owl_implementation = struct
  module M = Owl.Mat

  type vec = M.mat
  type mat = M.mat

  module Matrix = struct
    type t = mat
    let dim = M.shape
    let init size ~f = M.init_2d size size f

    let init_sym size ~f =
      let r = init size ~f:(fun _ _ -> 0.) in
      for i = 0 to size - 1 do
        M.set r i i (f i i) ;
        for j = i + 1 to size - 1 do
          let r_ij = f i j in
          M.set r i j r_ij ;
          M.set r j i r_ij
        done
      done ;
      r

    let diagm v = M.diagm v
    let dot a b = M.dot a b

    let row mat r = M.transpose (M.row mat r)

    let inplace_scal_mul f a = M.scalar_mul_ f a

    let scal_mul f a = M.scalar_mul f a

    let add a b = M.add a b
    let mul = M.mul
    let expm a = Owl.Linalg.D.expm a

    let log m = M.log m

    let max = M.max'

    let robust_equal ~tol:p m1 m2 =
      let diff = M.abs (add m1 (scal_mul (-1.) m2)) in (* substract two matrices *)
      max diff <= p

    let get m i j = M.get m i j
    let set m i j x = M.set m i j x

    let inverse m = M.inv m

    let apply m v = M.dot m v

    let pow x k =
      let m, n = M.shape x in
      if m <> n then invalid_arg "non-square matrix" ;
      if k < 0 then invalid_arg "negative power" ;
      let rec loop k =
        if k = 0 then M.eye m
        else if k mod 2 = 0 then
          let r = loop (k / 2) in
          M.dot r r
        else
          let r = loop ((k - 1) / 2) in
          M.dot x (M.dot r r)
      in
      loop k

    let transpose = M.transpose

    let diagonalize m =
      Owl_lapacke.syevr ~a:m ~jobz:'V' ~range:'A' ~vl:0. ~vu:0. ~il:0 ~iu:0 ~abstol:1e-6 ~uplo:'U'

    let%test "Owl Matrix.diagonalize" =
      let m = init 13 ~f:(fun i j ->
          float i +. float j
        )
      in
      let vp, p = diagonalize m in
      robust_equal
        ~tol:1e-6
        (dot p (dot (diagm vp) (transpose p)))
        m

    let zero_eigen_vector m =
      let n =
        let (m, n) = M.shape m in
        if m <> n then invalid_arg "Expected square matrix" else n
      in
      let a = M.(transpose (m :> mat) @= ones 1 n)
      and b = M.init_2d (n + 1) 1 (fun i _ -> if i < n then 0. else 1.) in
      Owl.Linalg.D.linsolve a b

    let of_arrays xs =
      let m = Array.length xs in
      if Array.for_all (fun row -> Array.length row = m) xs then
        Some (Owl.Mat.of_arrays xs)
      else None

    let of_arrays_exn xs =
      match of_arrays xs with
      | Some m -> m
      | None -> failwith "Arrays do not represent a matrix"

    let pp = Owl_pretty.pp_dsnda
  end

  module Vector = struct
    type t = vec
    let length x =
      match Owl.Arr.shape x with
      | [| n ; 1 |] -> n
      | _ -> assert false
    let init size ~f = M.init_2d size 1 (fun i _ -> f i)
    let map v ~f = M.map f v
    let scal_add = M.scalar_add
    let scal_mul = M.scalar_mul
    let inplace_scal_mul x y = M.scalar_mul_ x y
    let add v1 v2 = M.add v1 v2
    let mul v1 v2 = M.mul v1 v2
    let sum v = M.sum' v
    let log v = M.log v
    let exp v = M.exp v
    let min v = M.min' v
    let max v = M.max' v
    let get v i = M.get v i 0
    let set v i x = M.set v i 0 x
    let pp = Owl_pretty.pp_dsnda
    let of_array xs = init (Array.length xs) ~f:(fun i -> xs.(i))
    let to_array v = M.to_array v

    let robust_equal ~tol:p m1 m2 =
      let diff = M.abs (M.sub m1 m2) in (* substract two matrices *)
      max diff <= p

    let%test "Linear_algebra.Vec.{to,of}_array" =
      let xs = [| 1. ; 2. ; 3. |] in
      to_array (of_array xs) = xs
  end
end

module Lacaml = struct
  open Lacaml.D

  type mat = Lacaml.D.mat
  type vec = Lacaml.D.vec

  let inplace_scal_mat_mul f a = Mat.scal f a

  let scal_mat_mul f a =
    let r = lacpy a in
    inplace_scal_mat_mul f r ;
    r

  module Vector = struct
    type t = vec
    let length x = Vec.dim x
    let init size ~f = Vec.init size (fun i -> f (i - 1))
    let add v1 v2 = Vec.add v1 v2
    let mul v1 v2 = Vec.mul v1 v2
    let sum v = Vec.sum v
    let log v = Vec.log v
    let exp v = Vec.exp v
    let min v = Vec.min v
    let max v = Vec.max v
    let get v i = v.{i + 1}
    let set v i x = v.{i + 1} <- x
    let pp = pp_vec
    let to_array = Vec.to_array
    let of_array = Vec.of_array
    let scal_add s v = Lacaml.D.Vec.add_const s v
    let scal_mul s v =
      let r = copy v in
      scal s r ; r
    let inplace_scal_mul s v = scal s v
    let map v ~f = Vec.map f v
    let robust_equal ~tol:p v1 v2 =
      if length v1 <> length v2 then invalid_arg "incompatible dimensions" ;
      let diff = Vec.(abs (sub v1 v2)) in
      let relative_diff = (* element-wise diff/m1 *)
        mul diff (Vec.map (fun x -> 1./.x) v1)
      in
      Vec.max relative_diff <= p
  end

  module Matrix = struct
    type t = mat
    let dim m = Mat.dim1 m, Mat.dim2 m
    let init size ~f = Mat.init_rows size size (fun i j -> f (i - 1) (j - 1))

    let init_sym size ~f =
      let r = init size ~f:(fun _ _ -> 0.) in
      for i = 1 to size do
        r.{i, i} <- f (i - 1) (i - 1) ;
        for j = i + 1 to size do
          let r_ij = f (i - 1) (j - 1) in
          r.{i, j} <- r_ij ;
          r.{j, i} <- r_ij
        done
      done ;
      r

    let diagm v = Mat.of_diag v
    let add a b = Mat.add a b
    let norm1 x = lange ~norm:`O x
    let mul a b = Mat.mul a b
    let inplace_scal_mul f a = Mat.scal f a

    let scal_mul f a =
      let r = lacpy a in
      inplace_scal_mat_mul f r ;
      r

    let dot a b = gemm a b

    let apply m x = gemv m x

    let log m = Mat.log m

    let robust_equal ~tol:p m1 m2 =
      if Mat.dim1 m1 <> Mat.dim1 m2 || Mat.dim2 m1 <> Mat.dim2 m2
      then invalid_arg "incompatible dimensions" ;
      let diff = Mat.sub m1 m2 in
      lange ~norm:`M diff <= p

    let get m i j = m.{i + 1, j + 1}
    let set m i j x = m.{i + 1, j + 1} <- x
    let row mat r = Mat.copy_row mat (r + 1) (* FIXME: costly operation! *)

    let transpose m = Mat.transpose_copy m

    let diagonalize m =
      let tmp = lacpy m in (* copy matrix to avoid erasing original *)
      let _, v, c, _ = syevr ~vectors:true tmp in (* syevr = find eigenvalues and eigenvectors *)
      v, c

    let%test "Lapack Matrix.diagonalize" =
      let m = init 13 ~f:(fun i j ->
          float i +. float j
        )
      in
      let vp, p = diagonalize m in
      robust_equal
        ~tol:1e-6
        (dot p (dot (diagm vp) (transpose p)))
        m

    let pp = pp_mat

    let of_arrays_exn xs = Mat.of_array xs
    let of_arrays xs =
      try Some (of_arrays_exn xs)
      with _ -> None
    let inverse m =
      let tmp = lacpy m in (* copy matrix to avoid erasing original *)
      let tmp_vec = getrf tmp in (* getri requires a previous call to getrf (LU factorization) *)
      getri ~ipiv:tmp_vec tmp ; (* inversion *)
      tmp

    let zero_eigen_vector mat =
      let n = Mat.dim2 mat in
      if n <> Mat.dim1 mat then invalid_arg "Expected square matrix" ;
      let a = Mat.init_rows (n + 1) n (fun i j ->
          if i = n + 1 then 1. else mat.{j, i}
        )
      in
      let b = Mat.init_rows (n + 1) 1 (fun i _ -> if i <= n then 0. else 1.) in
      gels a b ;
      Vec.init n (fun i -> b.{i, 1})

    let pow x k =
      let m = Mat.dim1 x in
      let n = Mat.dim2 x in
      if m <> n then invalid_arg "non-square matrix" ;
      if k < 0 then invalid_arg "negative power" ;
      let rec loop k =
        if k = 0 then Mat.identity m
        else if k mod 2 = 0 then
          let r = loop (k / 2) in
          gemm r r
        else
          let r = loop ((k - 1) / 2) in
          gemm x (gemm r r)
      in
      loop k

    let rec naive_pow x k =
      let m = Mat.dim1 x in
      let n = Mat.dim2 x in
      if m <> n then invalid_arg "non-square matrix" ;
      if k < 0 then invalid_arg "negative power" ;
      if k = 0 then Mat.identity m
      else gemm x (naive_pow x (k - 1))

    let%test "lacaml matrix pow" =
      let m = Linear_algebra_tools.Lacaml.Mat.init 5 ~f:(fun i j -> float (i + j)) in
      robust_equal ~tol:1e-6 (pow m 13) (naive_pow m 13)

    let expm x =
      let m = Mat.dim1 x in
      let n = Mat.dim2 x in
      if m <> n then invalid_arg "matrix not square" ;
      (* trivial case *)
      if m = 1 && n = 1 then
        Mat.make 1 1 (Float.exp x.{1, 1})
      else (
        (* TODO: use gebal to balance to improve accuracy, refer to Julia's impl *)
        let xe = Mat.identity m in
        let norm_x = norm1 x in
        (* for small norm, use lower order Padé-approximation *)
        if norm_x <= 2.097847961257068 then (
          let c = (
            if norm_x > 0.9504178996162932 then
              [|17643225600.; 8821612800.; 2075673600.; 302702400.; 30270240.; 2162160.; 110880.; 3960.; 90.; 1.|]
            else if norm_x > 0.2539398330063230 then
              [|17297280.; 8648640.; 1995840.; 277200.; 25200.; 1512.; 56.; 1.|]
            else if norm_x > 0.01495585217958292 then
              [|30240.; 15120.; 3360.; 420.; 30.; 1.|]
            else
              [|120.; 60.; 12.; 1.|]
          ) in

          let x2 = gemm x x in
          let p = ref (lacpy xe) in
          let u = scal_mat_mul c.(1) !p in
          let v = scal_mat_mul c.(0) !p in

          for i = 1 to Array.(length c / 2 - 1) do
            let j = 2 * i in
            let k = j + 1 in
            p := gemm !p x2 ;
            Mat.axpy ~alpha:c.(k) !p u ;
            Mat.axpy ~alpha:c.(j) !p v ;
          done;

          let u = gemm x u in
          let a = Mat.sub v u in
          let b = Mat.add v u in
          gesv a b ;
          b
        )
        (* for larger norm, Padé-13 approximation *)
        else (
          let s = Owl_maths.log2 (norm_x /. 5.4) in
          let t = ceil s in
          let x = if s > 0. then scal_mul (2. ** (-. t)) x else x in

          let c =
            [|64764752532480000.; 32382376266240000.; 7771770303897600.;
              1187353796428800.;  129060195264000.;   10559470521600.;
              670442572800.;      33522128640.;       1323241920.;
              40840800.;          960960.;            16380.;
              182.;               1.|]
          in

          let x2 = gemm x x in
          let x4 = gemm x2 x2 in
          let x6 = gemm x2 x4 in
          let u =
            let m = lacpy x2 in
            inplace_scal_mat_mul c.(9) m ;
            Mat.axpy ~alpha:c.(11) x4 m ;
            Mat.axpy ~alpha:c.(13) x6 m ;
            let m = gemm x6 m in
            Mat.axpy ~alpha:c.(1) xe m ;
            Mat.axpy ~alpha:c.(3) x2 m ;
            Mat.axpy ~alpha:c.(5) x4 m ;
            Mat.axpy ~alpha:c.(7) x6 m ;
            gemm x m
          in
          let v =
            let m = lacpy x2 in
            inplace_scal_mat_mul c.(8) m ;
            Mat.axpy ~alpha:c.(10) x4 m ;
            Mat.axpy ~alpha:c.(12) x6 m ;
            let m = gemm x6 m in
            Mat.axpy ~alpha:c.(0) xe m ;
            Mat.axpy ~alpha:c.(2) x2 m ;
            Mat.axpy ~alpha:c.(4) x4 m ;
            Mat.axpy ~alpha:c.(6) x6 m ;
            m
          in
          let a = Mat.sub v u in
          let b = Mat.add v u in
          gesv a b ;

          let x = ref b in
          if s > 0. then (
            for _i = 1 to int_of_float t do
              x := gemm !x !x
            done;
          );
          !x
        )
      )

    let%test "lacaml expm 1d" =
      let c = 0.4 in
      let m = Mat.make 1 1 c in
      robust_equal ~tol:1e-6 (expm m) (Mat.make 1 1 (Float.exp c))

    let use_owl_matrix_fun m f =
        Mat.to_array m
        |> Owl.Mat.of_arrays
        |> f
        |> Owl.Mat.to_arrays
        |> Mat.of_array

    let%test "lacaml expm small norm" =
      let m = Linear_algebra_tools.Lacaml.Mat.init 5 ~f:(fun i j -> float (i + j) /. 10.) in
      let owl_expm = use_owl_matrix_fun m Owl.Linalg.D.expm in
      robust_equal ~tol:1e-6 (expm m) owl_expm

    let%test "lacaml expm large norm" =
      let m = Linear_algebra_tools.Lacaml.Mat.init 5 ~f:(fun i j -> float (i + j) /. 2.) in
      let owl_expm = use_owl_matrix_fun m Owl.Linalg.D.expm in
      robust_equal ~tol:1e-6 (expm m) owl_expm
  end
end

module Owl = Owl_implementation
