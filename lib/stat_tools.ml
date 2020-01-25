open Core_kernel

type sample_list = float list

let sample_branch_lengths ~(branchs:int->bool) ~(sampler:unit->float) tree () =
  Phylogenetic_tree.get_branch_lengths tree
  |> List.mapi ~f:(fun i l -> if branchs i then sampler () else l)
  |> Phylogenetic_tree.set_branch_lengths tree

let sample_list_of_file path =
  In_channel.read_lines path
  |> List.map ~f:(float_of_string)

let sample_list_extrema d =
  match
    List.min_elt ~compare:Float.compare d, List.max_elt ~compare:Float.compare d
  with
    (Some mi, Some ma) -> (mi, ma) | _ -> failwith "empty input distribution"

let sample_list_mean d =
  List.fold d ~init:(0., 0) ~f:(fun (s, c) x -> (s+.x, c+1))
  |> fun (s, c) -> s /. (float_of_int c)


(* ========== *)
(*  PLOTTING  *)
(* ========== *)
let bins ?(nb=20) d =
  let dmin, dmax = sample_list_extrema d in
  let bin_size = (dmax -. dmin)/.(float_of_int nb) in
  let count i = List.count d ~f:(
      fun x ->
        Float.(x > (float_of_int i)*.bin_size) &&
        Float.(x < ((float_of_int i)+.1.)*.bin_size)
    ) in
  List.init nb ~f:(
    fun x -> (float_of_int x +. 0.5) *. bin_size,
             (nb * count x |> float_of_int)
             /. (List.length d |> float_of_int |> ( *. ) (dmax -. dmin))
  )

let plot_sample_list ?(nb=20) d =
  let gp = Gnuplot.create () in
  Gnuplot.Series.lines_xy ~title:"Plot a line" ~color:`Blue (bins ~nb d)
  |> Gnuplot.plot gp

let plot_sample_lists ?(nb=20) l =
  let gp = Gnuplot.create () in
  List.mapi l ~f:(
    fun i x -> Gnuplot.Series.lines_xy
        ~title:(Printf.sprintf "distrib %d" i)
        (bins ~nb x)
  ) |> Gnuplot.plot_many gp

let pause () =
  ignore (In_channel.input_line In_channel.stdin : string option)
