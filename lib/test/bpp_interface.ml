open Core_kernel.Std

let felsenstein_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree seq =
  let script = Printf.sprintf
      "bppml \
       input.tree.file=%s/%s \
       input.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       output.tree.file=tmp.tree \
       optimization=None \
       > tmp.data"
      path tree path seq alphabet model
  in
  begin
    Out_channel.write_all "tmp.sh" ~data:script ;
    Sys.command "bash tmp.sh" |> ignore ;
    match
      In_channel.read_lines "tmp.data"
      |> List.filter ~f:(fun l->String.prefix l 11 = "Initial log")
    with
    | [l] ->
      Scanf.sscanf l "Initial log likelihood.................: %f" (fun x->x)
    | _ ->
      begin
        Printf.sprintf "ERROR (bpp_interface): Unexpected bppml output:\n%s"
          (In_channel.read_all "tmp.data") |> prerr_endline;
        failwith "Unexpected bppml output"
      end
  end

let seqgen_bpp ?(alphabet="DNA") ?(model="JC69") ?(path=".") ~tree output size =
  let script = Printf.sprintf
      "bppseqgen \
       input.tree.file=%s/%s \
       output.sequence.file=%s/%s \
       alphabet=%s \
       model=%s \
       number_of_sites=%d \
       > tmp.data"
      path tree path output alphabet model size
  in
  begin
    Out_channel.write_all "tmp.sh" ~data:script ;
    match Sys.command "bash tmp.sh" with
    | 0 -> ()
    | _ ->
      begin
        Printf.sprintf "ERROR (bpp_interface): bppseqgen failed with output:\n%s"
          (In_channel.read_all "tmp.data") |> prerr_endline;
        failwith "bppseqgen failed"
      end
  end

let test () = felsenstein_bpp
    ~path:"test_data"
    ~tree:"small_1.tree"
    "small_1.seq"

let test2 () = seqgen_bpp
    ~path:"test_data"
    ~tree:"small_1.tree"
    "tmp.seq"
    15
