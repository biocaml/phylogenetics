open Printf
open Lacaml.S

(* Linear algebra functions *)
type mat = Lacaml_float32.mat
type vec = Lacaml_float32.vec

let testMat = Mat.random 4 4

let testId () = Mat.init_cols 4 4 (fun x y -> if x=y then 1. else 0.)

let init size f = Mat.init_rows size size f

let initvec size f = Vec.init size f

let printMat mat = (pp_mat Format.std_formatter mat; printf "\n")

let printVec vec = (pp_vec Format.std_formatter vec; printf "\n")

let mult a ?alpha:(al=1.) b = gemm a b ~alpha:al

let mat_vec_mul m v = gemv m v

let vec_vec_add v1 v2 = Vec.add v1 v2

let vec_vec_mul v1 v2 = Vec.mul v1 v2

let sum_vec_elements  v = Vec.sum v

let rec pow a ?alpha:(al=1.) n =
  if n<=1 then a
  else if n=2 then mult a (pow a (n-1) ~alpha:al) ~alpha:al
  else mult a (pow a (n-1) ~alpha:al)

let sum a b = Mat.add a b

let rec fact n = if n=0 then 1 else n * (fact (n-1))

let exp a =
  let iterations = 15 in
  let rec aux i acc =
    if i<iterations then
      let factor = 1. /. (float_of_int (fact i)) in
      let newacc = sum (pow a i ~alpha:factor) acc in
      (aux (i+1) newacc)
    else
      acc
  in aux 1 a

let scalmul a f = (Mat.scal f a ; a)

(* test *)
let test () =
  let printline () = printf "==========================\n" in
  let myMat = testMat in printline () ;
  printMat myMat ; printline () ;
  printMat (mult myMat myMat ~alpha:178.); printline () ;
  printMat (pow myMat 4 ~alpha:378.); printline () ;
  printMat (scalmul (testId ()) 3.5)

let exptest () =
  let printline () = printf "==========================\n" in
  printline () ;
  printMat (exp (testId ())) ;
  printline () ;
