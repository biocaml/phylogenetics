include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: byte
	./test.byte

doc: default
	mkdir -p doc
	ocamlfind ocamldoc -package lacaml -I _build/lib/ lib/* -d doc -html

ca: clean
	rm -rf doc/
