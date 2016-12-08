include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: byte
	./_build/app/test_app.byte

doc: default
	mkdir -p doc
	ocamlfind ocamldoc -package lacaml -I _build/lib/ lib/* -d doc -html

ca: clean
	rm -rf doc/
