include $(shell opam config var solvuu-build:lib)/solvuu.mk

all: native byte doc

test: native
	./_build/app/test_app.native

bench: ./_build/app/bench_app.native
	./_build/app/bench_app.native

doc: default
	mkdir -p _build/doc
	ocamlfind ocamldoc -package lacaml,alcotest,core,gnuplot -I _build/lib/core -I _build/lib lib/test/* lib/core/*.mli lib/core/*.ml -d _build/doc -html

ca: clean
	rm -rf tmp* test_data/tmp*
