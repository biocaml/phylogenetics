# ================================================
#                      MISC
# ================================================
.PHONY: edit test bench explorer doc ca all


# ================================================
#                    BUILDING
# ================================================
include $(shell opam config var solvuu-build:lib)/solvuu.mk

all: native byte doc

edit: .ocamlinit .merlin ./_build/lib/biocaml_phylogeny_core.cma ./_build/lib/biocaml_phylogeny_test.cma

ca: clean
	rm -rf tmp* test_data/tmp*


# ================================================
#                     TESTING
# ================================================
test: ./_build/app/test_app.native
	$<

bench: ./_build/app/bench_app.native
	$<

explorer: ./_build/app/zipper_explorer.byte
	$<


# ================================================
#                   DOCUMENTATION
# ================================================
doc: default
	mkdir -p _build/doc
	ocamlfind ocamldoc -package lacaml,alcotest,core,gnuplot -I _build/lib/core -I _build/lib lib/test/* lib/core/*.mli lib/core/*.ml -d _build/doc -html
