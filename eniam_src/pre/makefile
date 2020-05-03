OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-tokenizer.cmxa eniam-morphology.cmxa eniam-subsyntax.cmxa eniam-plWordnet.cmxa eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa eniam-integration.cmxa eniam-lexSemantics.cmxa
INSTALLDIR=`ocamlc -where`

WAL= paths.ml
PRE= preProcessing.ml

all:
	$(OCAMLOPT) -o pre $(OCAMLOPTFLAGS) $(WAL) $(PRE)

generate:
	$(OCAMLOPT) -o wal_generate $(OCAMLOPTFLAGS) $(WAL) walGenerate.ml

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
	rm -f *~ *.cm[oix] *.o pre wal_generate
