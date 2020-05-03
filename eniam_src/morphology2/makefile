OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-morphology.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMmorphologyTypes.ml ENIAMmorphologyRules.ml ENIAMinflexion.ml

all: eniam-morphology.cma eniam-morphology.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-morphology.cmxa eniam-morphology.a eniam-morphology.cma $(INSTALLDIR)
	cp ENIAMmorphologyTypes.cmi ENIAMmorphologyRules.cmi ENIAMinflexion.cmi $(INSTALLDIR)
	cp ENIAMmorphologyTypes.cmx ENIAMmorphologyRules.cmx ENIAMinflexion.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/morphology
	cp resources/*  /usr/share/eniam/morphology

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-morphology.cmxa eniam-morphology.a eniam-morphology.cma $(INSTALLDIR)
	cp ENIAMmorphologyTypes.cmi ENIAMmorphologyRules.cmi ENIAMinflexion.cmi $(INSTALLDIR)
	cp ENIAMmorphologyTypes.cmx ENIAMmorphologyRules.cmx ENIAMinflexion.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/morphology
	cp resources/*  /usr/local/share/eniam/morphology

eniam-morphology.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-morphology.cma $(OCAMLFLAGS) $^

eniam-morphology.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-morphology.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test
