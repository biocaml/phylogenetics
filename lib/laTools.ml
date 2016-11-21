open Printf
open Lacaml.S

(* Linear algebra functions *)
module LATools :
sig
  type mat
  val testMat: mat
  val testId: mat
  val printMat: mat -> unit
  val mult: mat -> ?alpha:float -> mat -> mat
  val pow: mat ->  ?alpha:float -> int -> mat
  val sum: mat -> mat -> mat
  val exp: mat -> mat
end
=
struct
  type mat = Lacaml_float32.mat

  let testMat = Mat.random 4 4

  let testId = Mat.init_cols 4 4 (fun x y -> if x=y then 1. else 0.)

  let printMat mat = (pp_mat Format.std_formatter mat; printf "\n")

  let mult a ?alpha:(al=1.) b = gemm a b ~alpha:al

  let rec pow a ?alpha:(al=1.) n =
    if n<=1 then a
    else if n=2 then mult a (pow a (n-1) ~alpha:al) ~alpha:al
    else mult a (pow a (n-1) ~alpha:al)

  let sum a b = Mat.add a b

  let rec fact n = if n=0 then 1 else n * (fact (n-1))

  let exp a =
    let iterations = 10 in
    let rec aux i acc =
      if i<iterations then
        let factor = 1. /. (float_of_int (fact i)) in
        let newacc = sum (pow a i ~alpha:factor) acc in
        (aux (i+1) newacc)
      else
        acc
    in aux 1 a
end




(* test *)
let test () =
  let printline () = printf "\n==========================\n" in
  let myMat = LATools.testMat in
  LATools.printMat myMat ; printline () ;
  LATools.printMat (LATools.mult myMat myMat ~alpha:178.); printline () ;
  LATools.printMat (LATools.pow myMat 4 ~alpha:378.); printline () ;;

let exptest () =
  let printline () = printf "==========================\n" in
  printline () ;
  LATools.printMat (LATools.exp LATools.testId) ;
  printline () ;
;;

