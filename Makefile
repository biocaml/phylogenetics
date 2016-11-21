include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: byte
	./test.byte
