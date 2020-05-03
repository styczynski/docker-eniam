OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa eniam-lcg-parser.cmxa eniam-integration.cmxa xt.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAM_CONLL.ml CONLL_adapter.ml SkladnicaTypes.ml SkladnicaXmlToOcaml.ml SkladnicaTreeFinder.ml SkladnicaXmlToConll.ml xTToOcaml.ml ENIAMpreIntegration.ml

all: eniam-integration.cma eniam-integration.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-integration.cmxa eniam-integration.a eniam-integration.cma $(INSTALLDIR)
	cp ENIAM_CONLL.cmi CONLL_adapter.cmi ENIAMpreIntegration.cmi $(INSTALLDIR)
	cp ENIAM_CONLL.cmx CONLL_adapter.cmx ENIAMpreIntegration.cmx $(INSTALLDIR)
	# mkdir -p /usr/share/eniam/integration
	# cp resources/*  /usr/share/eniam/integration

install-local: install

eniam-integration.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-integration.cma $(OCAMLFLAGS) $^

eniam-integration.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-integration.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

# swigra_test: swigra_test.ml
# 	ocamlopt -o swigra_test $(OCAMLOPTFLAGS) swigra_test.ml


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
