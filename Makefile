include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: native
	./_build/app/test_app.native

bench: ./_build/app/bench_app.native
	./_build/app/bench_app.native

graph: bench
	gnuplot reject_branch.plt

doc: default
	mkdir -p _build/doc
	ocamlfind ocamldoc -package lacaml,biocaml,alcotest,core -I _build/lib/core -I _build/lib lib/test/* lib/core/sigs.ml lib/core/*.mli -d _build/doc -html

ca: clean
	rm -rf doc/ tmp* test_data/tmp*
