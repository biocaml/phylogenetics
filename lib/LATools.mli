type mat
val testMat: mat
val testId: unit -> mat
val init: int -> (int -> int -> float) -> mat
val printMat: mat -> unit
val mult: mat -> ?alpha:float -> mat -> mat
val pow: mat ->  ?alpha:float -> int -> mat
val sum: mat -> mat -> mat
val exp: mat -> mat
val scalmul: mat -> float -> mat
