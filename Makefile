MODULES = src/topoTree.cmo src/models.cmo

all: $(MODULES)

src/%.cmo: src/%.ml
	ocamlc -c $<

mytest: test.ml $(MODULES)
	ocamlc $^ -o $@

test: mytest
	@./mytest

clean :
	@rm -rf mytest src/*.cmi src/*.cmo _build/
