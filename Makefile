include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: native
	./_build/app/test_app.native

bug:
	rm -f .merlin .ocamlinit
	make .merlin
	make .ocamlinit

doc: default
	mkdir -p doc
	ocamlfind ocamldoc -package lacaml,biocaml -I _build/lib/core lib/core/* -d doc -html

ca: clean
	rm -rf doc/ tmp* test_data/tmp*
