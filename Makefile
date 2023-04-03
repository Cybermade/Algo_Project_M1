OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-package owl -package plplot -linkpkg

all: Project

Project: Project.ml
	$(OCAMLC) $(OCAMLFLAGS) -o Project Project.ml

clean:
	rm -f Project *.cmi *.cmo

run: Project
	./Project
