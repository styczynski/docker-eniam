OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa
OCAMLCFLAGS=$(INCLUDES) unix.cma xml-light.cma str.cma

#tu nalezy wpisac wszystkie moduly do skompilowania w kolejnosci zgodnej z zaleznosciami
MODS= NKJPxmlbasics.ml NKJP.mli NKJP.ml
TEST= test.ml

all:
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) $(MODS) $(TEST)

lib:
	$(OCAMLOPT) -linkall -a -o nkjp.cmxa $(INCLUDES) $(MODS)

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.mll.ml:
	ocamllex $<

.mly.mli:
	ocamlyacc $<

.mly.ml:
	ocamlyacc $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFALGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

clean:
	rm -f *~ *.cm[oix] *.o *.a res*.txt nkjp.cmxa
