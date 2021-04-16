open Phylogenetics

let () =
  Birth_death.age_ntaxa_simulation (Birth_death.make ~birth_rate:2. ~death_rate:1.) Gsl.Rng.(make (default ())) ~age:5. ~ntaxa:5
  |> Tree.map
    ~node:(fun () -> { Newick_ast.name = None })
    ~leaf:(fun i -> { Newick_ast.name = (Some (Printf.sprintf "n%d" i)) })
    ~branch:(fun l -> { Newick_ast.length = Some l ; tags = [] })
  |> (fun x -> Newick.Tree x)
  |> Newick.to_string
  |> print_endline
