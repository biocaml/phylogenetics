include $(shell opam config var solvuu-build:lib)/solvuu.mk

test: test.byte
	./test.byte
