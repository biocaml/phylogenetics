open Core
open Core_bench
open Phylogenetics

let a = Lacaml.D.Mat.hilbert 100

let a' =
  Lacaml.D.Mat.to_array a
  |> Owl.Mat.of_arrays

let lacaml_mat_mat_mul =
  Bench.Test.create
    ~name:"Lacaml_mat_mat_mul"
    (fun () -> Lacaml.D.gemm a a)

let owl_mat_mat_mul =
  Bench.Test.create
    ~name:"Owl_mat_mat_mul"
    (fun () -> Owl.Mat.dot a' a')

let b, b' =
  let n = 100 in
  Lacaml.D.Mat.init_rows n n (fun i j -> if i = j then -1. else 1. /. (float n -. 1.)),
  Linear_algebra.Mat.init n ~f:(fun i j -> if i = j then -1. else 1. /. (float n -. 1.))

let lacaml_stat_dist =
  Bench.Test.create
    ~name:"Lacaml_stat_dist"
    (fun () ->
       let open Lacaml.D in
       let stat_dist m =
         (* copy matrix to avoid erasing original *)
         let tmp = lacpy m in
         (* get eigenvector for eigenvalue 0 *)
         match syevr ~vectors:true ~range:(`V(-0.001,0.001)) tmp with
         | (_,_,c,_) ->
           let vec = Mat.col c 1 in
           (* normalize so the sum of elements equals 1 *)
           Linear_algebra_tools.Lacaml.(scal_vec_mul (1./.(Vec.sum vec)) vec)
       in
       stat_dist b
    )

let owl_stat_dist =
  Bench.Test.create
    ~name:"Owl_stat_dist"
    (fun () -> Linear_algebra.Mat.zero_eigen_vector b')

let lacaml_expm =
  Bench.Test.create ~name:"Lacaml_expm" (fun () -> Linear_algebra_tools.Lacaml.Mat.expm a)

let owl_expm =
  Bench.Test.create ~name:"Owl_expm" (fun () -> Owl.Linalg.D.expm a')

let v, v' =
  let n = Lacaml.D.Mat.dim1 a in
  Lacaml.D.Vec.init n float,
  (Linear_algebra.Vec.init n ~f:float :> Owl.Mat.mat)
    
let lacaml_mat_vec_mul =
  Bench.Test.create ~name:"Lacaml_mat_vec_mul" (fun () -> Linear_algebra_tools.Lacaml.mat_vec_mul a v)

let owl_mat_vec_mul =
  Bench.Test.create ~name:"Owl_mat_vec_mul" (fun () -> Owl.Mat.dot a' v')

let command = Command.group ~summary:"Performance benches" [
    "mat-mul", Bench.make_command [ lacaml_mat_mat_mul ; owl_mat_mat_mul ] ;
    "stat-dist", Bench.make_command [ lacaml_stat_dist ; owl_stat_dist ] ;
    "expm", Bench.make_command [lacaml_expm ; owl_expm] ;
    "mat-vec-mul", Bench.make_command [lacaml_mat_vec_mul ; owl_mat_vec_mul] ;
  ]

let () = Command.run command
