OCAMLOPTFLAGS=unix.cmxa

all: concraft_test.ml
	ocamlopt -o concraft_test $(OCAMLOPTFLAGS) concraft_test.ml

clean:
	rm -f *.cmi *.cmx *.o concraft_test
