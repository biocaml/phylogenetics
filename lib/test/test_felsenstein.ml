open Biocaml_phylogeny_core
open Alcotest
open Biocaml_ez
open Core_kernel.Std
open Felsenstein

(** Function used to compare floats and tolerate relative imprecision.
    Returns true if (1-p)*f1 < f2 < (1+p)*f1 *)
let float_compare p f1 f2 =
  let diff = f1-.f2 |> Pervasives.abs_float in
  diff/.(Pervasives.abs_float f1) <= p

let test_felsenstein_tiny () =
  JCFelsenstein.felsenstein () (TopoTree.of_preorder "0.1;0.1;0;1") (JCFelsenstein.Align.of_string_list ["C";"G"]) |>
  (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "expected log likelihoods" (-4.22471668644312)

let tree_small = TopoTree.of_preorder "0.21;0.1;0.3;0.4;0;0.8;0.1;1;2;0.12;0.9;3;0.2;0.3;0.3;0.4;4;5;6"
let seq_small  = ["C";"G";"C";"T";"A";"T";"G"] |> JCFelsenstein.Align.of_string_list
let ref_small = JCFelsenstein.felsenstein () tree_small seq_small

let test_felsenstein_small_normlog () =
  JCFelsenstein.felsenstein_logshift () tree_small seq_small |>
  (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "identical log likelihoods" ref_small

let test_felsenstein_small_normshift () =
  JCFelsenstein.felsenstein_shift () tree_small seq_small |>
  (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "identical log likelihoods" ref_small


type test_case = {
  name: string ;
  result: float ;
  tree: TopoTree.t ;
  seq: JCFelsenstein.Align.t ;
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
    seq = Printf.sprintf "%s/%s.seq" path name |> JCFelsenstein.Align.of_fasta ;
  }

  in
  Printf.sprintf "%s/%s" path file
  |> Core_kernel.Std.In_channel.read_lines
  |> List.filter ~f:(function "" -> false | s -> not (s.[0]='#'))
  |> aux path []


let test_of_case_list l f desc =
  l
  |> List.map ~f:(
    function {name; result; tree; seq} ->
      Printf.sprintf "felsenstein_%s (%s vs bio++)" name desc, `Quick,(
        fun () ->
          f () tree seq
          |> (check @@ testable (pp Alcotest.float) (float_compare 0.00001)) "identical log likelihood" result
      )
  )


let tests = [
  "felsenstein_tiny", `Quick, test_felsenstein_tiny ;
  "felsenstein_small (normal vs logshift)", `Quick, test_felsenstein_small_normlog ;
  "felsenstein_small (normal vs shift)", `Quick, test_felsenstein_small_normshift ;
] @ test_of_case_list (read_test_file "test_data" "tests") JCFelsenstein.felsenstein "normal"
  @ test_of_case_list (read_test_file "test_data" "tests") JCFelsenstein.felsenstein_shift "shift"
  @ test_of_case_list (read_test_file "test_data" "tests") JCFelsenstein.felsenstein_logshift "log shift"
