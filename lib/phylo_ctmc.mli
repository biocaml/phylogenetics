module type Alphabet = sig
  type t
  val card : int
  val to_int : t -> int
  type 'a vector
  val vector : (t -> 'a) -> 'a vector
end

module type Linalg = sig
  type vec
  type mat
  module Vec : sig
    type t = vec
    val get : t -> int -> float
    val init : int -> f:(int -> float) -> vec
    val mul : t -> t -> t
    val min : t -> float
    val max : t -> float
    val sum : t -> float
  end
  module Mat : sig
    type t = mat
    val row : t -> int -> vec
  end
  val scal_vec_mul : vec -> float -> vec
  val mat_vec_mul : mat -> vec -> vec
end

module Make(A : Alphabet)(L : Linalg) : sig
  open L
  type shifted_vector = SV of vec * float

  val pruning :
    ('a, 'b) Tree.t ->
    transition_matrix:(('a, 'b) Tree.branch -> L.mat) ->
    leaf_state:('a -> A.t) ->
    root_frequencies:vec ->
    float

  val conditionial_likelihoods :
    ('a, 'b) Tree.t ->
    transition_matrix:(('a, 'b) Tree.branch -> L.mat) ->
    leaf_state:('a -> A.t) ->
    (shifted_vector, mat) Tree.t

  val conditional_simulation :
    (shifted_vector, mat) Tree.t ->
    root_frequencies:vec ->
    choose:(Vec.t -> int) ->
    (int, mat) Tree.t
end
