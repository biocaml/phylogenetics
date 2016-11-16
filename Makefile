MODULES = topoTree models
CMOS = $(MODULES:%=src/%.cmo)

PACKAGES = lacaml
COMPILER = ocamlfind ocamlc -package $(PACKAGES)

all: mytest

src/%.cmo: src/%.ml
	$(COMPILER) -c $<

mytest: test.ml $(CMOS)
	$(COMPILER) -c -I src/ $<
	$(COMPILER) -o $@ -linkpkg $(CMOS) test.cmo

test: mytest
	@./mytest

clean :
	@rm -rf mytest src/*.cmi src/*.cmo _build/ *.cmi *.cmo
