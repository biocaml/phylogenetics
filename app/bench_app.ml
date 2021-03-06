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
  Linear_algebra.Owl.Matrix.init n ~f:(fun i j -> if i = j then -1. else 1. /. (float n -. 1.))

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
    (fun () -> Linear_algebra.Owl.Matrix.zero_eigen_vector b')

let lacaml_expm =
  let a =
    Lacaml.D.Mat.to_array a
    |> Linear_algebra.Lacaml.Matrix.of_arrays_exn
  in
  Bench.Test.create ~name:"Lacaml_expm" (fun () -> Linear_algebra.Lacaml.Matrix.expm a)

let owl_expm =
  Bench.Test.create ~name:"Owl_expm" (fun () -> Owl.Linalg.D.expm a')

let c, c' =
  let n = 4 in
  Lacaml.D.Mat.init_rows n n (fun i j -> if i = j then -1. else 1. /. (float n -. 1.)),
  (Linear_algebra.Owl.Matrix.init n ~f:(fun i j -> if i = j then -1. else 1. /. (float n -. 1.)) :> Owl.Mat.mat)

let v, v' =
  let n = Lacaml.D.Mat.dim1 c in
  Lacaml.D.Vec.init n float,
  (Linear_algebra.Owl.Vector.init n ~f:float :> Owl.Mat.mat)
    
let lacaml_mat_vec_mul =
  Bench.Test.create ~name:"Lacaml_mat_vec_mul" (fun () -> Linear_algebra_tools.Lacaml.mat_vec_mul c v)

let owl_mat_vec_mul =
  Bench.Test.create ~name:"Owl_mat_vec_mul" (fun () -> Owl.Mat.dot c' v')

let lacaml_vec_init =
  Bench.Test.create ~name:"Lacaml_vec_init" (fun () ->
      Linear_algebra_tools.Lacaml.Vec.init 100 ~f:(fun i -> if i = 42 then 1. else 0.)
    )

let owl_vec_init =
  Bench.Test.create ~name:"Owl_vec_init" (fun () ->
      Owl.Arr.init [|100|] (fun i -> if i = 42 then 1. else 0.)
    )

let command = Command.group ~summary:"Performance benches" [
    "mat-mul", Bench.make_command [ lacaml_mat_mat_mul ; owl_mat_mat_mul ] ;
    "stat-dist", Bench.make_command [ lacaml_stat_dist ; owl_stat_dist ] ;
    "expm", Bench.make_command [lacaml_expm ; owl_expm] ;
    "mat-vec-mul", Bench.make_command [lacaml_mat_vec_mul ; owl_mat_vec_mul] ;
    "vec-init", Bench.make_command [lacaml_vec_init ; owl_vec_init] ;
  ]

let () = Command.run command
