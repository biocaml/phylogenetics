include $(shell opam config var solvuu-build:lib)/solvuu.mk

all: byte inits .merlin

inits: .ocamlinit
	cp .ocamlinit lib/
	cp .ocamlinit app/

test: byte
	./test.byte

ca: clean
	rm -f lib/.ocamlinit
	rm -f app/.ocamlinit
