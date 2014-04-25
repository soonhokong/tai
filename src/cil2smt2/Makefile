ML=$(wildcard *.ml)
MLI=$(wildcard *.mli)

cil2smt.native: myocamlbuild.ml setup.ml setup.data $(ML) $(MLI)
	ocaml setup.ml -build

myocamlbuild.ml setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

clean:
	-rm -f myocamlbuild.ml setup.ml setup.data _tags *.native *.byte
