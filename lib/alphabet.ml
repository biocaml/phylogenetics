(** Integer-based representation of sequence alphabets *)

open Core

module type S = sig
  type t
  type vector
  type matrix
  type 'a table
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val all : t list
  val card : int
  val to_int : t -> int
  val counts : t Sequence.t -> int table
  val pp : Format.formatter -> t -> unit
  module Table : sig
    val init : (t -> 'a) -> 'a table
    val get : 'a table -> t -> 'a
    val set : 'a table -> t -> 'a -> unit
    val map : 'a table -> f:('a -> 'b) -> 'b table
    val map2 : 'a table -> 'a table -> f:('a -> 'a -> 'b) -> 'b table
    val mapi : 'a table -> f:(t -> 'a -> 'b) -> 'b table
    val of_array_exn : 'a array -> 'a table
    val of_vector : vector -> float table
    val choose : float table -> rng:Gsl.Rng.t -> t
    val fold : 'a table -> init:'b -> f:('b -> 'a -> 'b) -> 'b
    val foldi : 'a table -> init:'b -> f:(t -> 'b -> 'a -> 'b) -> 'b
    val count : 'a table -> f:('a -> bool) -> int
    val counti : 'a table -> f:(t -> 'a -> bool) -> int
  end
  module Vector : sig
    type symbol = t
    include Linear_algebra.Vector with type t = vector
    val init : (symbol -> float) -> vector
    val map : vector -> f:(float -> float) -> vector
    val mapi : vector -> f:(symbol -> float -> float) -> vector
    val map2 : vector -> vector -> f:(float -> float -> float) -> vector
    val fold : vector -> init:'a -> f:('a -> float -> 'a) -> 'a
    val foldi : vector -> init:'a -> f:(symbol -> 'a -> float -> 'a) -> 'a
    val iteri : vector -> f:(symbol -> float -> unit) -> unit
    val sum : vector -> float
    val maxi : t -> symbol * float
    val normalize : vector -> vector
    val of_array : float array -> vector option
    val of_array_exn : float array -> vector
    val upcast_exn : Linear_algebra.vec -> vector
    val get : t -> symbol -> float
    val set : t -> symbol -> float -> unit
  end
  val flat_profile : unit -> vector
  val random_profile : Gsl.Rng.t -> float -> vector
  module Matrix : sig
    type symbol = t
    include Linear_algebra.Matrix with type t = matrix
                                   and type vec := vector
    val init : (symbol -> symbol -> float) -> matrix
    val init_sym : (symbol -> symbol -> float) -> matrix
    val get : t -> symbol -> symbol -> float
    val set : t -> symbol -> symbol -> float -> unit
    val of_arrays : float array array -> matrix option
    val of_arrays_exn : float array array -> matrix
  end
  val ( .%() ) : vector -> t -> float
  val ( .%()<- ) : vector -> t -> float -> unit
  val ( .%{} ) : matrix -> t * t -> float
  val ( .%{}<- ) :  matrix -> t * t -> float -> unit
end

module type S_int = sig
  include S with type t = private int
             and type vector = private Linear_algebra.vec
             and type matrix = private Linear_algebra.mat
             and type 'a table = private 'a array
  val of_int : int -> t option
  val of_int_exn : int -> t
end

module Make(X : sig val card : int end) = struct
  type t = int
  include X
  let of_int i =
    if i < 0 || i >= card then None
    else Some i

  let of_int_exn n =
    if n < 0 || n >= card then raise (Invalid_argument "of_int_exn")
    else n

  let equal = Int.( = )
  let compare = Int.compare
  let all = List.init card ~f:Fn.id

  let counts xs =
    let r = Array.create ~len:card 0 in
    Sequence.iter ~f:(fun aa -> r.(aa) <- r.(aa) + 1) xs ;
    r

  type 'a table = 'a array
  module Table = struct
    let init f = Array.init card ~f
    let get xs a = xs.(a)
    let set xs a v = xs.(a) <- v
    let map = Array.map
    let map2 = Array.map2_exn
    let mapi = Array.mapi
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else a
    let of_vector v =
      let open Linear_algebra.Vector in
      Array.init (length v) ~f:(get v)
    let choose xs ~rng =
      Gsl.Randist.(discrete_preproc xs |> discrete rng)

    let fold = Array.fold
    let foldi = Array.foldi
    let count = Array.count
    let counti = Array.counti
  end
  module Vector = struct
    type symbol = t
    include Linear_algebra.Vector

    let iteri v ~f =
      for i = 0 to card - 1 do
        f i (get v i)
      done

    let init f = init card ~f
    let normalize v =
      let s = sum v in
      map v ~f:(fun x -> x /. s)
    let of_array_exn a =
      if Array.length a <> card then raise (Invalid_argument "vector_of_array_exn")
      else init (fun i -> a.(i))
    let of_array a =
      try Some (of_array_exn a)
      with _ -> None
    let upcast_exn a =
      let n = Linear_algebra.Vector.length a in
      if n = card
      then a
      else
        invalid_argf "vector_of_arr_exn: argument has shape %d" n ()

    let map2 x y ~f =
      init (fun i -> f (get x i ) (get y i))

    let mapi x ~f =
      init (fun i -> f i (get x i))

    let foldi (x:t) ~init ~f =
      let rec loop x i acc ~f =
        if i = card then acc else loop x (i+1) (f i acc (get x i)) ~f
      in loop x 0 init ~f

    let fold (x:t) ~init ~f =
      foldi x ~init ~f:(fun _ acc x -> f acc x)

    let counti (x:t) ~f =
      foldi x ~init:0 ~f:(fun i acc x -> if f i x then acc + 1 else acc)

    let count (x:t) ~f =
      counti x ~f:(fun _ x -> f x)

    (** Element with maximum value. In case of ties, it is the first one encountered *)
    let maxi (x:t) =
      let rec loop x i m =
        if i = card then m
        else
          let m = if Float.((get x i) > snd m) then i, get x i else m in
          loop x (i+1) m
      in loop x 1 (0, get x 0)

  end

  let flat_profile () =
    let theta = Float.(1. / of_int card) in
    Vector.init (fun _ -> theta)

  let random_profile rng alpha =
    let v = Table.init (fun _ -> 0.) in
    Gsl.Randist.dirichlet rng ~alpha:(Array.create ~len:card alpha) ~theta:v ;
    Vector.init (fun i -> v.(i))

  module Matrix = struct
    type symbol = t
    include Linear_algebra.Matrix

    let init f = init card ~f
    let init_sym f = init_sym card ~f
    let of_arrays_exn xs =
      let m = Array.length xs in
      if m = card then of_arrays_exn xs
      else failwith "Incorrect dimension"
    let of_array xs =
      let m = Array.length xs in
      if m = card then of_arrays xs
      else None
  end

  let to_int i = i
  type vector = Linear_algebra.vec

  let ( .%() ) v i = Vector.get v i
  let ( .%()<- ) v i x = Vector.set v i x
  type matrix = Linear_algebra.mat
  let ( .%{} ) m (i,j) = Matrix.get m i j
  let ( .%{}<- ) m (i, j) x = Matrix.set m i j x
  let pp = Format.pp_print_int
end
