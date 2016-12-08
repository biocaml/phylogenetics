open Printf
open Lacaml.S

(* Linear algebra functions *)
type mat = Lacaml_float32.mat
type vec = Lacaml_float32.vec

let test_mat = Mat.random 4 4

let test_id () = Mat.init_cols 4 4 (fun x y -> if x=y then (1.) else (0.))

let test_jc () = Mat.init_cols 4 4 (fun x y -> if x=y then (-.0.75) else (0.25))

let init_mat size f = Mat.init_rows size size f

let init_vec size f = Vec.init size f

let print_mat mat = pp_mat Format.std_formatter mat; printf "\n"

let print_vec vec = pp_vec Format.std_formatter vec; printf "\n"

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
  let rec aux i acc =
    if i<50 then
      let factor = 1. /. (float_of_int (fact i)) in
      let newacc = sum (pow a i ~alpha:factor) acc in
      (aux (i+1) newacc)
    else
      acc
  in aux 0 (Mat.make0 (Mat.dim1 a) (Mat.dim2 a))

let scal_mat_mult a f = (Mat.scal f a ; a)

(* test *)
let test () =
  let printline () = printf "==========================\n" in
  let myMat = test_mat in printline () ;
  print_mat myMat ; printline () ;
  print_mat (mult myMat myMat ~alpha:178.); printline () ;
  print_mat (pow myMat 4 ~alpha:378.); printline () ;
  print_mat (scal_mat_mult (test_id ()) 3.5)

let exptest ()  =
  let printline () = printf "==========================\n" in
  printline () ;
  print_mat (exp (test_id ())) ;
  printline () ;
  print_mat (exp (test_jc ())) ;
  printline () ;
  let mymat = Mat.init_cols 3 3 (fun x y ->
      if x=y || y=1 then 1. else 0.) in
  exp mymat |> print_mat
