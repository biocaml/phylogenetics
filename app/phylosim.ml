open Core

let command =
  let open Command.Let_syntax in
  Command.basic
    ~summary:"Simulation of alignments"
    (return (fun () -> ()))
