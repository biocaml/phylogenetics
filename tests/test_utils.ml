open Core_kernel
open Alcotest
open Phylogenetics.Stat_tools
module Sys = Caml.Sys
module Bppsuite = Phylogenetics.Bppsuite

let eps = 0.1

(* ============= *)
(*  COMPARISONS  *)
(* ============= *)

(* Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Float.abs in
  Float.(diff /. Float.abs f1 <= p)

let check_likelihood = (check @@ testable
                          (pp (Alcotest.float eps))
                          (float_compare 0.00001)
                       ) "identical log likelihoods!"

let check_distrib ref_estim d =
  (check @@ list (testable (pp (Alcotest.float eps)) (float_compare 0.05)))
    "Distributions with identical characteristics"
    ref_estim [sample_list_mean d]

let compare_matrices (type s) (module A : Phylogenetics.Alphabet.S with type matrix = s) =
  check @@ testable A.Matrix.pp (A.Matrix.robust_equal ~tol:0.0001)


(* ================ *)
(*  BPP INTERFACES  *)
(* ================ *)

(*  fails after printing the content of a file with a message*)
let fail_file ?(path="tmp.data") message =
  Printf.sprintf "ERROR (bpp_interface): %s:\n%s"
    message (In_channel.read_all path) |> prerr_endline; Out_channel.flush stdout;
  failwith message

let felsenstein_bpp ?(alphabet = Bppsuite.DNA) ?(model = Bppsuite.JC69) ?(path=".") ~tree seq =
  let call =
    Bppsuite.Cmd.bppml
      ~alphabet ~model
      ~input_tree_file:(Filename.concat path tree)
      ~input_sequence_file:(Filename.concat path seq)
      ()
  in
  let script = sprintf "%s > tmp.data 2>&1" call in
  match
    if Sys.command script <> 0 then fail_file "bppml failed" ;
    In_channel.read_lines "tmp.data"
    |> List.filter ~f:String.(fun l-> equal (prefix l 11) "Initial log")
  with (* looking for a very specific line *)
  | [l] ->
    Scanf.sscanf l "Initial log likelihood.................: %f" (fun x->x)
  | _ ->
    fail_file "unexpected bppml output"

let seqgen_bpp ?(alphabet = Bppsuite.DNA) ?(model = Bppsuite.JC69) ?(path=".") ~tree output size =
  let call =
    Bppsuite.Cmd.bppseqgen
      ~alphabet ~model
      ~input_tree_file:(Filename.concat path tree)
      ~output_sequence_file:(Filename.concat path output)
      ~number_of_sites:size
  in
  let script = sprintf "%s > tmp.data 2>&1" call in
  match Sys.command script with
  | 0 -> ()
  | _ -> fail_file "bppseqgen failed"
