MODULES = topoTree models
CMOS = $(MODULES:%=src/%.cmo)

all: mytest

src/%.cmo: src/%.ml
	ocamlc -c $<

mytest: test.ml $(CMOS)
	ocamlc -c -I src/ $<
	ocamlc $(CMOS) test.cmo -o $@

test: mytest
	@./mytest

clean :
	@rm -rf mytest src/*.cmi src/*.cmo _build/ *.cmi *.cmo
