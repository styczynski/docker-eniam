OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa eniam-integration.cmxa eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa eniam-lexSemantics.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= entries.ml ENIAMwalTypes.ml ENIAMwalStringOf.ml ENIAMwalParser.ml ENIAMwalReduce.ml ENIAMlexSemanticsTypes.ml ENIAMlexSemanticsData.ml ENIAMvalence.ml ENIAMwalRenderer.ml ENIAMadjuncts.ml \
    ENIAMlexSemanticsStringOf.ml ENIAMlexSemanticsHTMLof.ml ENIAMlexSemanticsXMLof.ml ENIAMplWordnet.ml ENIAMlexSemantics.ml

all: eniam-lexSemantics.cma eniam-lexSemantics.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lexSemantics.cmxa eniam-lexSemantics.a eniam-lexSemantics.cma $(INSTALLDIR)
	cp entries.cmi ENIAMwalTypes.cmi ENIAMwalStringOf.cmi ENIAMwalParser.cmi ENIAMwalReduce.cmi ENIAMlexSemanticsData.cmi ENIAMvalence.cmi ENIAMwalRenderer.cmi ENIAMadjuncts.cmi ENIAMlexSemanticsTypes.cmi ENIAMlexSemanticsStringOf.cmi ENIAMlexSemanticsHTMLof.cmi ENIAMlexSemanticsXMLof.cmi ENIAMplWordnet.cmi ENIAMlexSemantics.cmi $(INSTALLDIR)
	cp entries.cmx ENIAMwalTypes.cmx ENIAMwalStringOf.cmx ENIAMwalParser.cmx ENIAMwalReduce.cmx ENIAMlexSemanticsData.cmx ENIAMvalence.cmx ENIAMwalRenderer.cmx ENIAMadjuncts.cmx ENIAMlexSemanticsTypes.cmx ENIAMlexSemanticsStringOf.cmx ENIAMlexSemanticsHTMLof.cmx ENIAMlexSemanticsXMLof.cmx ENIAMplWordnet.cmx ENIAMlexSemantics.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/lexSemantics
	cp resources/*  /usr/share/eniam/lexSemantics
#	ln -s /usr/share/eniam/lexSemantics/proper_names_20160104.tab /usr/share/eniam/lexSemantics/proper_names.tab
#	ln -s /usr/share/eniam/lexSemantics/proper_names_sgjp_polimorf_20151020.tab /usr/share/eniam/lexSemantics/proper_names_sgjp_polimorf.tab

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lexSemantics.cmxa eniam-lexSemantics.a eniam-lexSemantics.cma $(INSTALLDIR)
	cp entries.cmi ENIAMwalTypes.cmi ENIAMwalStringOf.cmi ENIAMwalParser.cmi ENIAMwalReduce.cmi ENIAMlexSemanticsData.cmi ENIAMvalence.cmi ENIAMwalRenderer.cmi ENIAMadjuncts.cmi ENIAMlexSemanticsTypes.cmi ENIAMlexSemanticsStringOf.cmi ENIAMlexSemanticsHTMLof.cmi ENIAMlexSemanticsXMLof.cmi ENIAMplWordnet.cmi ENIAMlexSemantics.cmi $(INSTALLDIR)
	cp entries.cmx ENIAMwalTypes.cmx ENIAMwalStringOf.cmx ENIAMwalParser.cmx ENIAMwalReduce.cmx ENIAMlexSemanticsData.cmx ENIAMvalence.cmx ENIAMwalRenderer.cmx ENIAMadjuncts.cmx ENIAMlexSemanticsTypes.cmx ENIAMlexSemanticsStringOf.cmx ENIAMlexSemanticsHTMLof.cmx ENIAMlexSemanticsXMLof.cmx ENIAMplWordnet.cmx ENIAMlexSemantics.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/lexSemantics
	cp resources/*  /usr/local/share/eniam/lexSemantics
#	ln -s /usr/local/share/eniam/lexSemantics/proper_names_20160104.tab /usr/local/share/eniam/lexSemantics/proper_names.tab
#	ln -s /usr/local/share/eniam/lexSemantics/proper_names_sgjp_polimorf_20151020.tab /usr/local/share/eniam/lexSemantics/proper_names_sgjp_polimorf.tab

eniam-lexSemantics.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-lexSemantics.cma $(OCAMLFLAGS) $^

eniam-lexSemantics.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-lexSemantics.cmxa $(INCLUDES) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) $^

inttest: inttest.ml
	$(OCAMLOPT) -o inttest $(OCAMLOPTFLAGS) $^

interface: interface.ml
	$(OCAMLOPT) -o lexSemantics $(OCAMLOPTFLAGS) interface.ml


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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test inttest
