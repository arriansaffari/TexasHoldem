.PHONY: test

build:
	@echo "Compiling dune"
	@dune build

clean:
	@echo "Cleaning"
	@dune clean

play:
	@OCAMLRUNPARAM=b dune exec bin/main.exe

test:
	@echo "testing"
	@OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	@echo "Zipping"
	@dune clean
	@zip -r ocamlpoker.zip .

doc:
	dune build @doc
