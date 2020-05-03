OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES=entries.ml ENIAMwalTypes.ml ENIAMwalStringOf.ml ENIAMwalTEI.ml ENIAMwalConnect.ml ENIAMwalRealizations.ml ENIAMwalLex.ml

all: $(SOURCES) ENIAMwalGenerate.ml
	$(OCAMLOPT) -o converter $(OCAMLOPTFLAGS) $^

analyze: $(SOURCES) ENIAMwalAnalyze.ml
	mkdir -p results
	$(OCAMLOPT) -o analyze $(OCAMLOPTFLAGS) $^

install:
	mkdir -p /usr/share/eniam/Walenty
	cp resources/*  /usr/share/eniam/Walenty

install-local:
	mkdir -p /usr/local/share/eniam/Walenty
	cp resources/*  /usr/local/share/eniam/Walenty

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a converter analyze resources/*.tab results/*
