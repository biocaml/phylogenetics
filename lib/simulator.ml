(**
   Useful documents:
   - https://www.stat.wisc.edu/~larget/phylogeny.pdf
   - https://bmcevolbiol.biomedcentral.com/articles/10.1186/s12862-017-0979-y
   - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5854120/
*)
open Core_kernel

let memo f =
  let table = Caml.Hashtbl.create 253 in
  fun x ->
    match Caml.Hashtbl.find table x with
    | y -> y
    | exception Caml.Not_found ->
      let y = f x in
      Caml.Hashtbl.add table x y ;
      y

module type Evolution_model = sig
  type param
  type vector
  type matrix
  val stationary_distribution : param -> vector
  val rate_matrix : param -> matrix
end

module type Branch_info = sig
  type t
  val length : t -> float
end

module Make
    (A : Alphabet.S_int)
    (M : Evolution_model with type vector := A.vector
                          and type matrix := A.matrix)
    (BI : Branch_info) =
struct
  let symbol_sample v =
    Owl.Stats.categorical_rvs v
    |> A.of_int_exn

  let site_exponential_method tree ~(root : A.t) ~param =
    let rate_matrix = memo (fun branch -> M.rate_matrix (param branch)) in
    let transition_matrix b =
      A.Matrix.(expm (scal_mul (BI.length b) (rate_matrix b)))
    in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n b ->
        A.Matrix.row (transition_matrix b) (n :> int)
        |> A.Vector.to_array
        |> symbol_sample
      )

  (* Gillespie "first reaction" method *)
  let site_gillespie_first_reaction tree ~(root : A.t) ~param =
    let rate_matrix = memo (fun b -> M.rate_matrix (param b)) in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n b ->
        let rec loop state remaining_time =
          let waiting_times =
            A.Table.init (fun m ->
                if A.equal m state then (Float.infinity, m)
                else
                  let rate = (rate_matrix b).A.%{state, m} in
                  if Float.(rate < 1e-30) then (Float.infinity, m)
                  else
                    let tau = Owl.Stats.exponential_rvs ~lambda:rate in
                    tau, m
              )
          in
          let tau, next_state =
            Array.min_elt (waiting_times :> (float * A.t) array) ~compare:Poly.compare
            |> (fun x -> Option.value_exn x)
          in
          if Float.(tau > remaining_time) then state
          else loop next_state Float.(remaining_time - tau)
        in
        loop n (BI.length b)
      )

  (* Gillespie "direct" method *)
  let site_gillespie_direct tree ~(root : A.t) ~param =
    let codon_rates = memo (fun b -> M.rate_matrix (param b)) in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n b ->
        let rec loop state remaining_time =
          let rate_matrix = codon_rates b in
          let rates = A.Table.init (fun m -> if A.equal m state then 0. else rate_matrix.A.%{state, m}) in
          let total_rate = Owl.Stats.sum (rates :> float array) in
          let tau = Owl.Stats.exponential_rvs ~lambda:total_rate in
          if Float.(tau > remaining_time) then state
          else
            let next_state = symbol_sample (rates :> float array) in
            (* assert (state <> next_state) ; *)
            loop next_state Float.(remaining_time - tau)
        in
        loop n (BI.length b)
      )

  let sequence_gillespie_direct rng tree ~update_iterator ~root ~rate_vector =
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun seq b ->
        let state = Array.copy seq in
        let n = Array.length state in
        let rate i = rate_vector b state i in
        let rates = Array.init n ~f:rate in
        let pos_rate i = Owl.Stats.sum (rates.(i) : float A.table :> float array) in
        let pos_rates = Discrete_pd.init n ~f:pos_rate in
        let rec loop remaining_time =
          let total_rate = Discrete_pd.total_weight pos_rates in
          let tau = Gsl.Randist.exponential rng ~mu:total_rate in
          if Float.(tau > remaining_time) then state
          else
            let pos = Discrete_pd.draw pos_rates rng in
            let next_letter = symbol_sample (rates.(pos) :> float array) in
            state.(pos) <- next_letter ;
            update_iterator ~n ~pos (fun pos ->
                rates.(pos) <- rate pos ;
                Discrete_pd.update pos_rates pos (pos_rate pos)
              ) ;
            loop Float.(remaining_time - tau)
        in
        loop (BI.length b)
      )

  let hmm0 ~len ~dist =
    Array.init len ~f:(fun i -> symbol_sample (dist i : float A.table :> float array))
end

module Mutsel(BI : Branch_info) = struct
  include Make(Mutsel.NSCodon)(Mutsel)(BI)

  let alignment tree ~root param =
    List.init (Array.length root) ~f:(fun i ->
        site_gillespie_direct tree ~root:root.(i) ~param:(param i)
        |> Tree.leaves
        |> List.map ~f:Codon.Universal_genetic_code.NS.to_string
      )
    |> List.transpose_exn
    |> List.map ~f:String.concat
    |> List.map ~f:Dna.of_string_unsafe
end
