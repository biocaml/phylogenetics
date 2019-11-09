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
  type 'a vector
  type 'a matrix
  type rate_matrix = private float matrix
  val stationary_distribution : param -> float vector
  val rate_matrix : param -> rate_matrix
end

module Make
    (A : Alphabet.S_int)
    (M : Evolution_model with type 'a vector := 'a A.vector
                          and type 'a matrix := 'a A.matrix) =
struct
  let symbol_sample v =
    Owl.Stats.categorical_rvs (v : float A.vector :> float array)
    |> A.of_int_exn

  let site_exponential_method tree ~(root : A.t) ~param =
    let rate_matrix = memo (fun condition -> M.rate_matrix (param condition)) in
    let transition_matrix (branch_length, condition) =
      let rates = (rate_matrix condition :> float array array) in
      Rate_matrix.transition_probability_matrix ~tau:branch_length ~rates
    in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n b ->
        (transition_matrix b).((n :> int))
        |> A.vector_of_array_exn
        |> symbol_sample
      )

  (* Gillespie "first reaction" method *)
  let site_gillespie_first_reaction tree ~(root : A.t) ~param =
    let rate_matrix = memo (fun condition -> M.rate_matrix (param condition)) in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n (branch_length, condition) ->
        let rec loop state remaining_time =
          let waiting_times =
            A.Vector.init (fun m ->
                if m = state then (Float.infinity, m)
                else
                  let rate = (rate_matrix condition :> float A.matrix).A.%{state, m} in
                  if rate < 1e-30 then (Float.infinity, m)
                  else
                    let lambda = 1. /. rate in
                    let tau = Owl.Stats.exponential_rvs ~lambda in
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
        loop n branch_length
      )

  (* Gillespie "direct" method *)
  let site_gillespie_direct tree ~(root : A.t) ~param =
    let codon_rates = memo (fun condition -> M.rate_matrix (param condition)) in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun n (branch_length, condition) ->
        let rec loop state remaining_time =
          let rate_matrix = codon_rates condition in
          let rates = A.Vector.init (fun m -> if m = state then 0. else (rate_matrix :> float A.matrix).A.%{state, m}) in
          let total_rate = A.Vector.reduce ~f:( +. ) rates in
          let tau = Owl.Stats.exponential_rvs ~lambda:(1. /. total_rate) in
          if Float.(tau > remaining_time) then state
          else
            let next_state = symbol_sample rates in
            (* assert (state <> next_state) ; *)
            loop next_state Float.(remaining_time - tau)
        in
        loop n branch_length
      )

  let sequence_gillespie_direct tree ~root ~param =
    let codon_rates = memo M.rate_matrix in
    Tree.propagate tree ~init:root ~node:Fn.const ~leaf:Fn.const ~branch:(fun seq (branch_length, condition) ->
        let rec loop state remaining_time =
          let rate_matrices = Array.mapi state ~f:(fun i _ -> codon_rates (param state i condition)) in
          let rates =
            Array.mapi rate_matrices ~f:(fun i mat ->
                A.Vector.init (fun m ->
                    if m = state.(i) then 0. else (mat :> float A.matrix).A.%{state.(i), m}
                  )
              ) in
          let pos_rates = Array.map rates ~f:(A.Vector.reduce ~f:( +. )) in
          let total_rate = Array.reduce_exn pos_rates ~f:( +. ) in
          let tau = Owl.Stats.exponential_rvs ~lambda:(1. /. total_rate) in
          if Float.(tau > remaining_time) then state
          else
            let pos = Owl.Stats.categorical_rvs pos_rates in
            let next_letter = symbol_sample rates.(pos) in
            let next_state =
              let t = Array.copy seq in
              t.(pos) <- next_letter ;
              t
            in
            loop next_state Float.(remaining_time - tau)
        in
        loop seq branch_length
      )

  let hmm0 ~len ~dist =
    Array.init len ~f:(fun i -> symbol_sample (dist i))
end

module Mutsel = struct
  include Make(Mutsel.NSCodon)(Mutsel)

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
