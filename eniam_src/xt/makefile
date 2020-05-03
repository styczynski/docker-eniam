OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-lcg-parser.cmxa xt.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES=xTTypes.ml xTStringOf.ml xTXmlOf.ml xTContext.ml xTPrologParser.ml xTDisambiguation.ml xTNormalizer.ml xTOfXml.ml xTLinearTermOf.ml

all: xt.cmxa xt.cma

install: all
	mkdir -p $(INSTALLDIR)
	cp xt.cmxa xt.a xt.cma $(INSTALLDIR)
	cp xTTypes.cmi xTStringOf.cmi xTXmlOf.cmi xTContext.cmi xTPrologParser.cmi xTDisambiguation.cmi xTNormalizer.cmi xTOfXml.cmi xTLinearTermOf.cmi $(INSTALLDIR)
	cp xTTypes.cmx xTStringOf.cmx xTXmlOf.cmx xTContext.cmx xTPrologParser.cmx xTDisambiguation.cmx xTNormalizer.cmx xTOfXml.cmx xTLinearTermOf.cmx $(INSTALLDIR)

install-local: all
	mkdir -p $(INSTALLDIR)
	cp xt.cmxa xt.a xt.cma $(INSTALLDIR)
	cp xTTypes.cmi xTStringOf.cmi xTXmlOf.cmi xTContext.cmi xTPrologParser.cmi xTDisambiguation.cmi xTNormalizer.cmi xTOfXml.cmi xTLinearTermOf.cmi $(INSTALLDIR)
	cp xTTypes.cmx xTStringOf.cmx xTXmlOf.cmx xTContext.cmx xTPrologParser.cmx xTDisambiguation.cmx xTNormalizer.cmx xTOfXml.cmx xTLinearTermOf.cmx $(INSTALLDIR)

xt.cma: $(SOURCES)
	ocamlc -linkall -a -o xt.cma $(OCAMLFLAGS) $^

xt.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o xt.cmxa $(INCLUDES) $^

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
