open Core_kernel.Std
open Gnuplot
open Pareto

type sample_list = float list

let sample_float_uniform ?(min=0.0) max () =
  let my_dist = Distributions.Uniform.create ~lower:min ~upper:max in
  Distributions.Uniform.quantile my_dist ~p:(Random.float 1.0)
  (* (Distributions.Uniform.sample ~size:1 my_dist).(0) *)

let sample_branch_lengths ~(branchs:int->bool) ~(sampler:unit->float) tree () =
  TopoTree.get_branch_lengths tree
  |> List.mapi ~f:(fun i l -> if branchs i then sampler () else l)
  |> TopoTree.set_branch_lengths tree

let sample_list_of_file path =
  In_channel.read_lines path
  |> List.map ~f:(float_of_string)

let sample_list_extrema d =
  match
    List.min_elt ~cmp:Float.compare d, List.max_elt ~cmp:Float.compare d
  with
    (Some mi, Some ma) -> (mi, ma) | _ -> failwith "empty input distribution"


(* ========== *)
(*  PLOTTING  *)
(* ========== *)
let bins ?(nb=20) d =
  let dmin, dmax = sample_list_extrema d in
  let bin_size = (dmax -. dmin)/.(float_of_int nb) in
  let count i = List.count d ~f:(
      fun x ->
        (x > (float_of_int i)*.bin_size) &&
        (x < ((float_of_int i)+.1.)*.bin_size)
    ) in
  List.init nb ~f:(
    fun x -> (float_of_int x +. 0.5) *. bin_size,
            (nb * count x |> float_of_int)
            /. (List.length d |> float_of_int |> ( *. ) (dmax -. dmin))
  )

let plot_sample_list ?(nb=20) d =
  let gp = Gp.create () in
  Series.lines_xy ~title:"Plot a line" ~color:`Blue (bins ~nb d)
  |> Gp.plot gp

let plot_sample_lists ?(nb=20) l =
  let gp = Gp.create () in
  List.mapi l ~f:(
    fun i x -> Series.lines_xy
        ~title:(Printf.sprintf "distrib %d" i)
        (bins ~nb x)
  ) |> Gp.plot_many gp

let pause () =
  In_channel.input_line In_channel.stdin |> ignore
