# README #

Currently implemented:
* topology tree data structure w/ branch length and indexes at leaves
    * parser from post-order enumeration
    * newick parser
* data structures for bases, sequences and sequence tables;
    * hashtable implem for alignments (+ legacy list implem)
    * list implem for sequences
* linear algebra functions (half wrappers around lacaml functions and half new functions);
* evolution models (w/ static distribution and transition matrix diagonalization):
    * JC69
    * K80
    * model generation from transition matrix (poor performance so far)
* felsenstein pruning w/ underflow avoidance
* sequence generation from model
* distribution plotting using gnuplot
* tests that compare results to bio++
    * interface for bppml and bppseqgen for validating test results
