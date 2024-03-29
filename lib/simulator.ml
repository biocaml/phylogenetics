(**
   Useful documents:
   - https://www.stat.wisc.edu/~larget/phylogeny.pdf
   - https://bmcevolbiol.biomedcentral.com/articles/10.1186/s12862-017-0979-y
   - https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5854120/
*)
open Core

module type Branch_info = sig
  type t
  val length : t -> float
end

module Make
    (A : Alphabet.S_int)
    (BI : Branch_info) =
struct
  let symbol_sample rng v =
    Gsl.Randist.discrete_preproc v
    |> Gsl.Randist.discrete rng
    |> A.of_int_exn

  let transition_matrix rate_matrix b =
    A.Matrix.(expm (scal_mul (BI.length b) (rate_matrix b)))

  let evolve tree ~f ~root =
    Tree.propagate tree ~init:root
      ~node:(fun x_in ni -> x_in, (ni, x_in))
      ~leaf:(fun x_in li -> li, x_in)
      ~branch:(fun x_in bi ->
        let x_out = f bi x_in in
        x_out, bi
      )

  let site_exponential_method rng tree ~root ~transition_matrix =
    evolve tree ~root ~f:(fun bi (x_in : A.t) ->
        A.Matrix.row (transition_matrix bi) (x_in :> int)
        |> A.Vector.to_array
        |> symbol_sample rng
      )

  (* Gillespie "first reaction" method *)
  let site_gillespie_first_reaction rng tree ~root ~rate_matrix =
    evolve tree ~root ~f:(fun bi (x_in : A.t) ->
        let rate_matrix = rate_matrix bi in
        let rec loop state remaining_time =
          let waiting_times =
            A.Table.init (fun m ->
                if A.equal m state then (Float.infinity, m)
                else
                  let rate = rate_matrix.A.%{state, m} in
                  if Float.(rate < 1e-30) then (Float.infinity, m)
                  else
                    let tau = Gsl.Randist.exponential rng ~mu:(1. /. rate) in
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
        loop x_in (BI.length bi)
      )

  (* Gillespie "direct" method *)
  let branch_gillespie_direct rng ~start_state ~rate_matrix ~branch_length ~init ~f =
    let rec loop state t acc =
      let rates = A.Table.init (fun m -> if A.equal m state then 0. else rate_matrix.A.%{state, m}) in
      let total_rate = -. rate_matrix.A.%{state, state} in
      let t' = t +. Gsl.Randist.exponential rng ~mu:(1. /. total_rate) in
      if Float.(t' > branch_length) then acc
      else
        let next_state = symbol_sample rng (rates :> float array) in
        loop next_state t' (f acc next_state t')
    in
    loop start_state 0. init

  let site_gillespie_direct rng tree ~(root : A.t) ~rate_matrix =
    evolve tree ~root ~f:(fun bi (x_in : A.t) ->
        let rate_matrix = rate_matrix bi in
        branch_gillespie_direct rng
          ~start_state:x_in ~rate_matrix ~branch_length:(BI.length bi)
          ~init:x_in ~f:(fun _ n _ -> n)
      )

  let sequence_gillespie_direct rng tree ~update_iterator ~root ~rate_vector =
    evolve tree ~root ~f:(fun bi seq ->
        let state = Array.copy seq in
        let n = Array.length state in
        let rate i = rate_vector bi state i in
        let rates = Array.init n ~f:rate in
        let pos_rate i = Utils.array_sum (rates.(i) : float A.table :> float array) in
        let pos_rates = Discrete_pd.init n ~f:pos_rate in
        let rec loop remaining_time =
          let total_rate = Discrete_pd.total_weight pos_rates in
          let tau = Gsl.Randist.exponential rng ~mu:(1. /. total_rate) in
          if Float.(tau > remaining_time) then state
          else
            let pos = Discrete_pd.draw pos_rates rng in
            let next_letter = symbol_sample rng (rates.(pos) :> float array) in
            state.(pos) <- next_letter ;
            update_iterator ~n ~pos (fun pos ->
                rates.(pos) <- rate pos ;
                Discrete_pd.update pos_rates pos (pos_rate pos)
              ) ;
            loop Float.(remaining_time - tau)
        in
        loop (BI.length bi)
      )

  let hmm0 rng ~len ~dist =
    Array.init len ~f:(fun i -> symbol_sample rng (dist i : float A.table :> float array))
end

module NSCodon(BI : Branch_info) = struct
  include Make(Mutsel.NSCodon)(BI)

  let alignment rng tree ~root ~rate_matrix =
    List.init (Array.length root) ~f:(fun i ->
        let rate_matrix = rate_matrix i in
        site_gillespie_direct rng tree ~root:root.(i) ~rate_matrix
        |> Tree.leaves
        |> List.map ~f:(fun (_, c) -> Codon.Universal_genetic_code.NS.to_string c)
      )
    |> List.transpose_exn
    |> List.map ~f:String.concat
    |> List.map ~f:Dna.of_string_unsafe
end
