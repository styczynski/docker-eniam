OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa \
	eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa \
	eniam-lexSemantics.cmxa #eniam-semantics.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMsemTypes.ml ENIAMsemLexicon.ml ENIAMsemValence.ml ENIAMdisambiguation.ml ENIAMcoreference.ml ENIAMsemStringOf.ml ENIAMsemXMLof.ml ENIAMsemGraph.ml ENIAMsemLatexOf.ml ENIAMsemGraphOf.ml

all: eniam-semantics.cma eniam-semantics.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-semantics.cmxa eniam-semantics.a eniam-semantics.cma $(INSTALLDIR)
	cp ENIAMsemTypes.cmi ENIAMsemLexicon.cmi ENIAMsemValence.cmi ENIAMdisambiguation.cmi ENIAMcoreference.cmi ENIAMsemStringOf.cmi ENIAMsemGraph.cmi ENIAMsemXMLof.cmi ENIAMsemLatexOf.cmi ENIAMsemGraphOf.cmi $(INSTALLDIR)
	cp ENIAMsemTypes.cmx ENIAMsemLexicon.cmx ENIAMsemValence.cmx ENIAMdisambiguation.cmx ENIAMcoreference.cmx ENIAMsemStringOf.cmx ENIAMsemGraph.cmx ENIAMsemXMLof.cmx ENIAMsemLatexOf.cmx ENIAMsemGraphOf.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/semantics
	cp resources/*  /usr/share/eniam/semantics

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-semantics.cmxa eniam-semantics.a eniam-semantics.cma $(INSTALLDIR)
	cp ENIAMsemTypes.cmi ENIAMsemLexicon.cmi ENIAMsemValence.cmi ENIAMdisambiguation.cmi ENIAMcoreference.cmi ENIAMsemStringOf.cmi ENIAMsemGraph.cmi ENIAMsemXMLof.cmi ENIAMsemLatexOf.cmi ENIAMsemGraphOf.cmi $(INSTALLDIR)
	cp ENIAMsemTypes.cmx ENIAMsemLexicon.cmx ENIAMsemValence.cmx ENIAMdisambiguation.cmx ENIAMcoreference.cmx ENIAMsemStringOf.cmx ENIAMsemGraph.cmx ENIAMsemXMLof.cmx ENIAMsemLatexOf.cmx ENIAMsemGraphOf.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/semantics
	cp resources/*  /usr/local/share/eniam/semantics

eniam-semantics.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-semantics.cma $(OCAMLFLAGS) $^

eniam-semantics.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-semantics.cmxa $(INCLUDES) $^

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a
