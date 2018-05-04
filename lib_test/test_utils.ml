open Core
open Alcotest
open Phylogenetics.Linear_algebra_tools
open Phylogenetics.Stat_tools

let eps = 0.1

(* ============= *)
(*  COMPARISONS  *)
(* ============= *)

(* Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

let check_likelihood = (check @@ testable
                          (pp (Alcotest.float eps))
                          (float_compare 0.00001)
                       ) "identical log likelihoods!"

let check_distrib ref_estim d =
  (check @@ list (testable (pp (Alcotest.float eps)) (float_compare 0.05)))
    "Distributions with identical characteristics"
    ref_estim [sample_list_mean d]

let compare_matrices = check @@ testable pp_mat (compare 0.0001)


(* ================ *)
(*  BPP INTERFACES  *)
(* ================ *)

(*  fails after printing the content of a file with a message*)
let fail_file ?(path="tmp.data") message =
  Printf.sprintf "ERROR (bpp_interface): %s:\n%s"
    message (In_channel.read_all path) |> prerr_endline; Out_channel.flush stdout;
  failwith message

let felsenstein_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree seq =
  let script = Printf.sprintf
      "bppml \
       input.tree.file=%s/%s \
       input.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       output.tree.file=tmp.tree \
       optimization=None \
       > tmp.data 2>&1"
      path tree path seq alphabet model
  in
  match
    if Sys.command script <> 0 then fail_file "bppml failed" ;
    In_channel.read_lines "tmp.data"
    |> List.filter ~f:(fun l->String.prefix l 11 = "Initial log")
  with (* looking for a very specific line *)
  | [l] ->
    Scanf.sscanf l "Initial log likelihood.................: %f" (fun x->x)
  | _ ->
    fail_file "unexpected bppml output"

let seqgen_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree output size =
  let script = Printf.sprintf
      "bppseqgen \
       input.tree.file=%s/%s \
       output.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       number_of_sites=%d \
       > tmp.data 2>&1"
      path tree path output alphabet model size
  in match Sys.command script with
  | 0 -> ()
  | _ -> fail_file "bppseqgen failed"
