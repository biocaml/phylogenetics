module Make_simulator
    (A : Alphabet.S_int)
    (BI : Simulator.Branch_info) :
sig
  type site = Site of (A.t, (A.t * int) option, branch_info) Tree.t
  and branch_info = {
    original_branch_info : BI.t ;
    length : float ;
    insertions : (A.t, (A.t * int) option, branch_info) Tree.branch list
  }

  val site_gillespie_direct :
    Gsl.Rng.t ->
    (_, _, BI.t) Tree.t ->
    root:A.t ->
    rate_matrix:(BI.t -> A.matrix) ->
    rates_upon_insertion:(BI.t -> float array) ->
    lambda:float ->
    mu:float ->
    site

  val fold_alignment :
    site ->
    init:'a ->
    f:('a -> row:int -> col:int -> A.t -> 'a) ->
    'a

  val alignment_of_site :
    (A.t -> char) ->
    _ Tree.t ->
    site ->
    string Array.t
end

