include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: byte
	./test.byte

doc: default
	mkdir -p doc
	ocamlfind ocamldoc -I _build/lib -package lacaml lib/*

ca: clean
	rm -rf doc/
