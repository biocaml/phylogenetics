open Core_kernel.Std

let felsenstein_bpp ?(model="JC69") ?(path=".") ~tree seq =
  let fulltree = Printf.sprintf "%s/%s" path tree in
  let fullseq = Printf.sprintf "%s/%s" path seq in
  let script = Printf.sprintf
      "bppml \
       input.tree.file=%s \
       input.sequence.file=%s \
       alphabet=DNA \
       model=%s \
       output.tree.file=tmp.tree \
       optimization=None \
       > tmp.data"
      fulltree fullseq model
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



let test () = felsenstein_bpp
    ~path:"test_data"
    ~tree:"small_1.tree"
    "small_1.seq"
