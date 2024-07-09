(lang dune 3.6)
(using menhir 2.0)
(generate_opam_files true)

(name phylogenetics)
(source (github biocaml/phylogenetics))
(homepage "https://github.com/biocaml/phylogenetics/")
(bug_reports "https://github.com/biocaml/phylogenetics/issues")
(license CeCILL-B)
(authors
  "Louis Duchemin"
  "Vincent Lanore"
  "Corentin Moumard"
  "Philippe Veber")
(maintainers "philippe.veber@gmail.com")

(package
  (name phylogenetics)
  (synopsis "Algorithms and datastructures for phylogenetics")
  (tags (bioinformatics evolution phylogeny))
  (depends
    (alcotest :with-test)
    angstrom-unix
    binning
    (biotk (>= 0.2.0))
    (core (>= v0.16.0))
    dune
    gsl
    (lacaml (>= 10.0.2))
    menhir
    ppx_deriving
    (printbox (>= 0.6.1))
    printbox-text
    (yojson (>= 1.6.0))))
