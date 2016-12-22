open Core_kernel.Std

let felsenstein_bpp ?(model="JC69") path tree seq =
  let fulltree = Printf.sprintf "%s/%s" path tree in
  let fullseq = Printf.sprintf "%s/%s" path seq in
  let script = Printf.sprintf
      "bppml input.tree.file=%s input.sequence.file=%s alphabet=DNA model=%s output.tree.file=tmp.tree optimization=None > tmp.data"
      fulltree fullseq model
  in
  Out_channel.write_all "tmp.sh" ~data:script ;
  Sys.command "bash tmp.sh" |> ignore ;
  let line =
    In_channel.read_lines "tmp.data"
    |> List.filter ~f:(fun l->String.prefix l 11 = "Initial log")
  in match line with
  | [l] -> Scanf.sscanf l "Initial log likelihood.................: %f" (fun x->x)
  | _ -> failwith "Unexpected bppml output"

  let test () = felsenstein_bpp "test_data" "small_1.tree" "small_1.seq";;
