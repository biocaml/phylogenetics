open Printf
open Lacaml.D

(* Linear algebra functions *)
type mat = Lacaml_float64.mat
type vec = Lacaml_float64.vec

let init_mat size f = Mat.init_rows size size f

let init_diag v = Mat.of_diag v

let init_vec size f = Vec.init size f

let pp_mat = pp_mat

let pp_vec = pp_vec

let mult a ?alpha:(al=1.) b = gemm a b ~alpha:al

let mat_vec_mul m v = gemv m v

let vec_vec_add v1 v2 = Vec.add v1 v2

let vec_vec_mul v1 v2 = Vec.mul v1 v2

let sum_vec_elements  v = Vec.sum v

let rec pow a ?alpha:(al=1.) n =
  if n=0 then
    Mat.init_cols (Mat.dim1 a) (Mat.dim2 a) (fun x y -> if x=y then 1.0 else 0.0)
  else if n=1 then a
  else if n=2 then pow a (n-1) ~alpha:al |> mult a ~alpha:al
  else pow a (n-1) ~alpha:al |> mult a

let sum a b = Mat.add a b

let rec fact n = if n=0 || n=1 then 1 else n * (fact (n-1))

let exp a =
  (* matrix exponentiation using the series *)
  let rec aux i acc =
    if i<15 then
      let factor = 1. /. (float_of_int (fact i)) in
      let newacc = sum (pow a i ~alpha:factor) acc in
      (aux (i+1) newacc)
    else
      acc
  in aux 0 (Mat.make0 (Mat.dim1 a) (Mat.dim2 a))

let log_mat m = Mat.log m

let log_vec v = Vec.log v

let unlog_vec v = Vec.exp v

let scal_mat_mul a f = (Mat.scal f a ; a)

let scal_vec_mul v s = (scal s v ; v)

let scal_vec_mul_cpy v s = (let tmp = copy v in scal s tmp ; tmp)

let scal_vec_add v s = Vec.add_const s v

let min_vec v = Vec.min v

let max_vec v = Vec.max v

let compare p m1 m2 =
  let diff = sum m1 (scal_mat_mul m2 (-1.)) in (* substract two matrices *)
  let relative_diff = (* element-wise diff/m1 *)
    mult diff (Mat.map (fun x -> 1./.x) m1)
    |> Mat.abs
  in
  let maxdiff = Mat.fold_cols (fun acc vec -> max acc (max_vec vec)) 0.0 relative_diff in
  maxdiff <= p

let get_vec v i = v.{i}
let get_mat m i j = m.{i,j}

let inverse m =
  let tmp = lacpy m in (* copy matrix to avoid erasing original *)
  let tmp_vec = getrf tmp in (* getri requires a previous call to getrf (LU factorization) *)
  getri ~ipiv:tmp_vec tmp ; (* inversion *)
  tmp

let diagonalize m =
  let tmp = lacpy m in (* copy matrix to avoid erasing original *)
  match syevr ~vectors:true tmp with (* syevr = find eigenvalues and eigenvectors *)
  | (_,v,c,_) -> c, v, Mat.transpose_copy c (* extract only the relevant data *)

let stat_dist m =
  (* copy matrix to avoid erasing original *)
  let tmp = lacpy m in
  (* get eigenvector for eigenvalue 0 *)
  match syevr ~vectors:true ~range:(`V(-0.001,0.001)) tmp with
  | (_,_,c,_) ->
    let vec = Mat.col c 1 in
    (* normalize so the sum of elements equals 1 *)
    scal_vec_mul vec (1./.(sum_vec_elements vec))


