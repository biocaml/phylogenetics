open Lacaml.S

(* Linear algebra functions *)
module LATools :
sig
  type mat
  val testMat: mat
  val printMat: mat -> unit
  val mult: mat -> mat -> mat
  val pow: mat -> int -> mat
end
=
struct
  type mat = Lacaml_float32.mat
  let testMat = Mat.random 4 4
  let printMat = pp_mat Format.std_formatter
  let mult a b = gemm a b
  let rec pow a n = if n=1 then a else gemm a (pow a (n-1))
end




(* test *)
let myMat = LATools.testMat ;;
LATools.printMat myMat ;;
LATools.printMat (LATools.mult myMat myMat);;
LATools.printMat (LATools.pow myMat 4);;

