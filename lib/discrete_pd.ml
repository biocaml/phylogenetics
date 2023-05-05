open Core

type t = {
  shift : int ;
  weights : float array ;
}

let is_leaf dpd i = i >= dpd.shift

let init n ~f =
  let shift = Float.(to_int (2. ** round_up (log (float n) /. log 2.))) - 1 in
  let m = shift + n in
  let weights = Array.create ~len:m 0. in
  for i = 0 to n - 1 do
    weights.(shift + i) <- f (i)
  done ;
  for i = shift - 1 downto 0 do
    if 2 * i + 1 < m then weights.(i) <- weights.(2 * i + 1) ;
    if 2 * i + 2 < m then weights.(i) <- weights.(i) +. weights.(2 * i + 2)
  done ;
  { shift ; weights }

let draw dpd rng =
  let x = dpd.weights.(0) *. Gsl.Rng.uniform rng in
  let rec loop acc i =
    if is_leaf dpd i then i
    else if Float.( >= ) (acc +. dpd.weights.(2 * i + 1)) x then
      loop acc (2 * i + 1)
    else loop (acc +. dpd.weights.(2 * i + 1)) (2 * i + 2)
  in
  loop 0. 0 - dpd.shift

let update dpd i w_i =
  let m = Array.length dpd.weights in
  let j = i + dpd.shift in
  dpd.weights.(j) <- w_i ;
  let rec loop k =
    dpd.weights.(k) <- dpd.weights.(2 * k + 1) ;
    if 2 * k + 2 < m then dpd.weights.(k) <- dpd.weights.(k) +. dpd.weights.(2 * k + 2) ;
    if k > 0 then loop ((k - 1) / 2)
  in
  loop ((j - 1) / 2)

let total_weight dpd = dpd.weights.(0)

let demo ~n ~ncat =
  let rng = Gsl.Rng.(make (default ())) in
  let probs = Array.init ncat ~f:(fun _ -> Gsl.Rng.uniform rng) in
  let sum = Array.fold probs ~init:0. ~f:( +. ) in
  let pd = init ncat ~f:(fun _ -> 0.) in
  let counts = Array.create ~len:ncat 0 in
  Array.iteri probs ~f:(update pd) ;
  for _ = 1 to n do
    let k  = draw pd rng in
    counts.(k) <- counts.(k) + 1
  done ;
  Array.map probs ~f:(fun x -> x /. sum),
  Array.map counts ~f:(fun k -> float k /. float n)
