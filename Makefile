all: mytest

mytest: main.ml
	@ocamlc $< -o $@

test: mytest
	@./mytest

clean :
	@rm -f mytest *.cmi *.cmo *.cmx *.o
