OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMsubsyntaxTypes.ml ENIAMsubsyntaxStringOf.ml ENIAMsubsyntaxHTMLof.ml ENIAMsubsyntaxXMLof.ml ENIAMsubsyntaxGraphOf.ml ENIAMpaths.ml ENIAM_MWE.ml ENIAMsentences.ml ENIAMconcraft.ml ENIAMsubsyntax.ml

all: eniam-subsyntax.cma eniam-subsyntax.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp ENIAMsubsyntaxTypes.cmi ENIAMsubsyntaxStringOf.cmi ENIAMsubsyntaxHTMLof.cmi ENIAMsubsyntaxXMLof.cmi ENIAMsubsyntaxGraphOf.cmi ENIAMpaths.cmi ENIAM_MWE.cmi ENIAMsentences.cmi ENIAMsubsyntax.cmi $(INSTALLDIR)
	cp ENIAMsubsyntaxTypes.cmx ENIAMsubsyntaxStringOf.cmx ENIAMsubsyntaxHTMLof.cmx ENIAMsubsyntaxXMLof.cmx ENIAMsubsyntaxGraphOf.cmx ENIAMpaths.cmx ENIAM_MWE.cmx ENIAMsentences.cmx ENIAMsubsyntax.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/subsyntax
	cp resources/*  /usr/share/eniam/subsyntax

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-subsyntax.cmxa eniam-subsyntax.a eniam-subsyntax.cma $(INSTALLDIR)
	cp ENIAMsubsyntaxTypes.cmi ENIAMsubsyntaxStringOf.cmi ENIAMsubsyntaxHTMLof.cmi ENIAMsubsyntaxXMLof.cmi ENIAMsubsyntaxGraphOf.cmi ENIAMpaths.cmi ENIAM_MWE.cmi ENIAMsentences.cmi ENIAMsubsyntax.cmi $(INSTALLDIR)
	cp ENIAMsubsyntaxTypes.cmx ENIAMsubsyntaxStringOf.cmx ENIAMsubsyntaxHTMLof.cmx ENIAMsubsyntaxXMLof.cmx ENIAMsubsyntaxGraphOf.cmx ENIAMpaths.cmx ENIAM_MWE.cmx ENIAMsentences.cmx ENIAMsubsyntax.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/subsyntax
	cp resources/*  /usr/local/share/eniam/subsyntax

eniam-subsyntax.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-subsyntax.cma $(OCAMLFLAGS) $^

eniam-subsyntax.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-subsyntax.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

interface: interface.ml
	$(OCAMLOPT) -o subsyntax $(OCAMLOPTFLAGS) interface.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test subsyntax
