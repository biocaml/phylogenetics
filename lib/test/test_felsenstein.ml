open Biocaml_phylogeny_core
open Alcotest
open Biocaml_ez
open Core_kernel.Std
open Felsenstein.JCFelsenstein

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

let check_likelihood = (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "identical log likelihoods"

let test_felsenstein_tiny () =
  felsenstein ~site:0 () () (TopoTree.of_preorder "0.1;0.1;0;1") (Align.of_string_list ["C";"G"]) |>
  check_likelihood (-4.22471668644312)

type test_case = {
  name: string ;
  result: float ;
  tree: TopoTree.t ;
  seq: Align.t ;
}

let read_test_file path file =
  let open Core_kernel.Std.In_channel in
  let rec aux path acc = function
    | name::range::models::t ->
      aux path ((list_cases path name (Scanf.sscanf range "%d-%d" (fun x y->x,y)) (String.split models ~on:' '))@acc) t
    | _ -> acc

  and list_cases path name (rmin, rmax) lmodels =
    List.range rmin (rmax+1)
    |> List.map ~f:(fun i->Printf.sprintf "%s_%d" name i)
    |> List.cartesian_product lmodels
    |> List.map ~f:(function (x,y)->build_case path y x )

  and build_case path name model = {
    name = name ;
    result = Printf.sprintf "%s/%s.%s" path name model |> read_all
             |> String.strip |> float_of_string ;
    tree = Printf.sprintf "%s/%s.tree" path name |> TopoTree.of_newick_file ;
    seq = Printf.sprintf "%s/%s.seq" path name |> Align.of_fasta ;
  }

  in
  Printf.sprintf "%s/%s" path file
  |> Core_kernel.Std.In_channel.read_lines
  |> List.filter ~f:(function "" -> false | s -> not (s.[0]='#'))
  |> aux path []
  |> List.rev


let test_of_case_list l f desc =
  List.map l ~f:(
    function {name; result; tree; seq} ->
      Printf.sprintf "felsenstein_%s (%s vs bio++)" name desc, `Quick,(
        fun () ->
          f () tree seq
          |> check_likelihood result
      )
  )

let test_of_filename file =
 test_of_case_list (read_test_file "test_data" file)

let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny ;
] @ test_of_filename "test_single_small" (felsenstein ~site:0 ()) "normal"
  @ test_of_filename "test_single_small" (felsenstein_shift ~site:0 ()) "shift"
  @ test_of_filename "test_single_small" (felsenstein_logshift ~site:0 ()) "log shift"
  @ test_of_filename "test_single_big" (felsenstein_shift ~site:0 ()) "shift"
  @ test_of_filename "test_single_big" (felsenstein_logshift ~site:0 ()) "log shift"
  @ test_of_filename "test_multi" (multi_felsenstein ()) "normal"
  @ test_of_filename "test_multi" (multi_felsenstein_shift ()) "shift"
  @ test_of_filename "test_multi" (multi_felsenstein_logshift ()) "log shift"
