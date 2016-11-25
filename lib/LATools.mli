type mat
type vec
val testMat: mat
val testId: unit -> mat
val init: int -> (int -> int -> float) -> mat
val initvec: int -> (int -> float) -> vec
val printMat: mat -> unit
val printVec: vec -> unit
val mult: mat -> ?alpha:float -> mat -> mat
val pow: mat ->  ?alpha:float -> int -> mat
val sum: mat -> mat -> mat
val exp: mat -> mat
val scalmul: mat -> float -> mat
val test: unit -> unit
val exptest: unit -> unit
