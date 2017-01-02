# README #

Currently implemented:
* topology tree data structure w/ branch length and indexes at leaves
    * parser from post-order enumeration
    * newick parser
* data structures for bases, sequences and sequence tables;
    * hashtable implem for alignments (+ legacy list implem)
    * list implem for sequences
* linear algebra functions (half wrappers around lacaml functions and half new functions);
* evolution models:
    * JC69
    * K80 (untested)
* felsenstein pruning w/ underflow avoidance
* sequence generation from model
* tests that compare results to bio++
    * interface for bppml and bppseqgen for validating test results
