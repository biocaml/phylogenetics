open Core_kernel.Std
open Gnuplot

let sample_float_uniform ?(min=0.0) max () = Random.float (max -. min) +. min

let sample_branch_lengths ~(branchs:int->bool) ~(sampler:unit->float) tree () =
  TopoTree.get_branch_lengths tree
  |> List.mapi ~f:(fun i l -> if branchs i then sampler () else l)
  |> TopoTree.set_branch_lengths tree

type distrib = float list

let distrib_of_file path =
  In_channel.read_lines path
  |> List.map ~f:(float_of_string)

let distrib_extrema d =
  match
    List.min_elt ~cmp:Float.compare d, List.max_elt ~cmp:Float.compare d
  with
    (Some mi, Some ma) -> (mi, ma) | _ -> failwith "empty input distribution"

let bins ?(nb=10) d =
  let dmin, dmax = distrib_extrema d in
  let bin_size = (dmax -. dmin)/.(float_of_int nb) in
  let count i = List.count d ~f:(
      fun x ->
        (x > (float_of_int i)*.bin_size) &&
        (x < ((float_of_int i)+.1.)*.bin_size)
    ) in
  List.init nb ~f:(
    fun x-> (nb * count x |> float_of_int)
            /. (List.length d |> float_of_int)
  )

let plot_distrib ?(nb=10) d =
  let gp = Gp.create () in
  Series.lines ~title:"Plot a line" ~color:`Blue (bins ~nb d)
  |> Gp.plot gp

let plot_distribs ?(nb=10) l =
  let gp = Gp.create () in
  List.mapi l ~f:(
    fun i x -> Series.lines
        ~title:(Printf.sprintf "distrib %d" i)
        (bins ~nb x)
  ) |> Gp.plot_many gp

let pause () =
  In_channel.input_line In_channel.stdin |> ignore

let test () =
  let data = distrib_of_file "tmp_post.txt" in
  plot_distrib data

let test2 () =
  plot_distribs [
    distrib_of_file "tmp_prior.txt" ;
    distrib_of_file "tmp_post.txt"
  ]
