MODULES = topoTree models
MLLIBS = $(MODULES:%=lib/%.ml)

PACKAGES = 'lacaml'
OCB = ocamlbuild -pkgs $(PACKAGES)


all: test.native

test.native: $(MLLIBS)
	$(OCB) -I src -I lib $@

test: test.native
	./test.native

clean:
	ocamlbuild -clean
