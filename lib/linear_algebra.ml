module M = Owl.Mat

type vec = M.mat
type mat = M.mat

let mat_vec_mul m v = M.dot m v

module Mat = struct
  type t = mat
  let init size ~f = M.init_2d size size f
  let diagm v = M.diagm v
  let dot a b = M.dot a b

  let row mat r = M.row mat r

  let inplace_scal_mul f a = M.scalar_mul_ f a

  let scal_mul f a = M.scalar_mul f a

  let add a b = M.add a b
  let mul = M.mul
  let expm a = Owl.Linalg.D.expm a

  let log m = M.log m

  let max = M.max'

  let compare ~tol:p m1 m2 =
    let diff = add m1 (scal_mul (-1.) m2) in (* substract two matrices *)
    let relative_diff = (* element-wise diff/m1 *)
      mul diff (M.map (fun x -> 1./.x) m1)
      |> M.abs
    in
    max relative_diff <= p

  let get m i j = M.get m i j

  let inverse m = M.inv m

  let diagonalize m =
    Owl_lapacke.syevr ~a:m ~jobz:'V' ~range:'A' ~vl:0. ~vu:0. ~il:0 ~iu:0 ~abstol:1e-6 ~uplo:'U'

  let pp = Owl_pretty.pp_dsnda
end

module Vec = struct
  type t = vec
  let init size ~f = M.init_2d size 1 (fun i _ -> f i)
  let scal_add = M.scalar_add
  let scal_mul = M.scalar_mul
  let inplace_scal_mul x y = M.scalar_mul_ x y
  let add v1 v2 = M.add v1 v2
  let mul v1 v2 = M.mul v1 v2
  let sum v = M.sum' v
  let log v = M.log v
  let exp v = M.exp v
  let min v = M.min' v
  let max v = M.max' v
  let get v i = M.get v 1 i
  let pp = Owl_pretty.pp_dsnda
end
