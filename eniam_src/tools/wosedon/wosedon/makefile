OCAMLOPTFLAGS=unix.cmxa

all: wosedon_test.ml
	ocamlopt -o wosedon_test $(OCAMLOPTFLAGS) wosedon_test.ml

clean:
	rm -f *.cmi *.cmx *.o wosedon_test
