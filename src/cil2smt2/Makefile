all: setup.ml setup.data cil2smt.native

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

cil2smt.native:
	ocaml setup.ml -build

clean:
	-rm -f myocamlbuild.ml setup.ml setup.data _tags *.native *.byte
