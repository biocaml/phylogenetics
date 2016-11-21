open Lacaml.S

(* Linear algebra functions *)
module LATools :
sig
  type mat
  val testMat: mat
  val printMat: mat -> unit
  val mult: mat -> ?alpha:float -> mat -> mat
  val pow: mat -> int -> mat
  (* val sum: mat -> mat -> mat *)
  (* val pow_scal: int -> mat -> int -> mat *)
end
=
struct
  type mat = Lacaml_float32.mat
  let testMat = Mat.random 4 4
  let printMat = pp_mat Format.std_formatter
  let mult a ?alpha:(al=1.) b =  gemm a b ~alpha:al
  let rec pow a n = if n<=1 then a else gemm a (pow a (n-1))
  let rec fact n = if n=0 then 1 else n * (fact (n-1))
end




(* test *)
let myMat = LATools.testMat ;;
LATools.printMat myMat ;;
LATools.printMat (LATools.mult myMat myMat ~alpha:178.);;
LATools.printMat (LATools.pow myMat 4);;

