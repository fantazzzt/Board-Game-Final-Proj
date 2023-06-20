build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

risk:
	OCAMLRUNPARAM=b dune exec bin/main.exe

clean:
	dune clean

doc:
	dune build @doc
