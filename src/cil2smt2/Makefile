cil2smt.native: basic.ml cil2smt.ml myocamlbuild.ml setup.ml setup.data smt2_cmd.ml
	ocaml setup.ml -build

myocamlbuild.ml setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

clean:
	-rm -f myocamlbuild.ml setup.ml setup.data _tags *.native *.byte
