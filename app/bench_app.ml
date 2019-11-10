open Core
open Core_bench

let a = Lacaml.D.Mat.hilbert 100

let a' =
  Lacaml.D.Mat.to_array a
  |> Owl.Mat.of_arrays

let lacaml_mat_mat_mul_test =
  Bench.Test.create
    ~name:"Lacaml_mat_mat_mul"
    (fun () -> Lacaml.D.gemm a a)

let owl_mat_mat_mul_test =
  Bench.Test.create
    ~name:"Owl_mat_mat_mul"
    (fun () -> Owl.Mat.dot a' a')

let tests = [
  lacaml_mat_mat_mul_test ;
  owl_mat_mat_mul_test ;
]

let command = Bench.make_command tests

let () = Command.run command
