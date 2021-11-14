# Phylogenetics

This library provides algorithms and datastructures to perform
inferences in molecular evolution. It features:
- typed representation of DNA/amino-acid/codon alphabets
- rate matrices and probability transition matrices for various
  evolution models (JC69, K80, GTR, Mutsel)
- site-independent Gillespie simulators
- a simulator for gapped alignments under the TKF91 model
- a tree simulator under the birth-death model
- parsers for Newick, NHX and phylip formats
- a GSL-based implementation of the pruning algorithm, with underflow
  avoidance

## Installation

Using [opam](http://opam.ocaml.org/), simply type

```
opam install phylogenetics
```

to install the library, or:

```
opam pin add -y phylogenetics --dev-repo
```
to get the current development version.
