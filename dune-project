(lang dune 2.0)
(using menhir 2.0)
(generate_opam_files true)

(name phylogenetics)
(source (github biocaml/phylogenetics))
(homepage "https://github.com/biocaml/phylogenetics/")
(bug_reports "https://github.com/biocaml/phylogenetics/issues")
(license CeCILL-B)
(authors
  "Vincent Lanore"
  "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name phylogenetics)
  (synopsis "Algorithms and datastructures for phylogenetics")
  (tags (bioinformatics evolution phylogeny))
  (depends
    angstrom-unix
    (biocaml (>= 0.8.0))
    (core (>= v0.14.0))
    (dune (>= 1.11))
    (gnuplot (>= 0.2.0))
    gsl
    (lacaml (>= 10.0.2))
    menhir
    (ocaml-r (>= 0.2.0))
    owl
    ppx_deriving
    printbox))
