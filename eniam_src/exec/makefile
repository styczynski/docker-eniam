OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam -I ../corpora
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa xt.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa eniam-lcg-parser.cmxa eniam-integration.cmxa \
	eniam-lcg-lexicon.cmxa \
	eniam-lexSemantics.cmxa eniam-semantics.cmxa eniam-exec.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMexecTypes.ml ENIAMexec.ml ENIAMselectSent.ml ENIAMvisualization.ml ENIAMexecXMLof.ml

all: eniam-exec.cma eniam-exec.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-exec.cmxa eniam-exec.a eniam-exec.cma $(INSTALLDIR)
	cp ENIAMexecTypes.cmi ENIAMexec.cmi ENIAMselectSent.cmi ENIAMexecXMLof.cmi ENIAMvisualization.cmi $(INSTALLDIR)
	cp ENIAMexecTypes.cmx ENIAMexec.cmx ENIAMselectSent.cmx ENIAMexecXMLof.cmx ENIAMvisualization.cmx $(INSTALLDIR)

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-exec.cmxa eniam-exec.a eniam-exec.cma $(INSTALLDIR)
	cp ENIAMexecTypes.cmi ENIAMexec.cmi ENIAMselectSent.cmi ENIAMexecXMLof.cmi ENIAMvisualization.cmi $(INSTALLDIR)
	cp ENIAMexecTypes.cmx ENIAMexec.cmx ENIAMselectSent.cmx ENIAMexecXMLof.cmx ENIAMvisualization.cmx $(INSTALLDIR)

eniam-exec.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-exec.cma $(OCAMLFLAGS) $^

eniam-exec.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-exec.cmxa $(INCLUDES) $^

parser: parser.ml
	$(OCAMLOPT) -o parser $(OCAMLOPTFLAGS) $^

test: test.ml
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) $^

mergertest: parserMerger.ml mergertest.ml
	$(OCAMLOPT) -o mergertest $(OCAMLOPTFLAGS) $^

semparser: semparser.ml
	mkdir -p results
	$(OCAMLOPT) -o semparser $(OCAMLOPTFLAGS) $^

webInterface: webInterface.ml
	$(OCAMLOPT) -o parser3.cgi $(OCAMLOPTFLAGS) $^

webInterface2: webInterface2.ml
	$(OCAMLOPT) -o parser4.cgi $(OCAMLOPTFLAGS) $^
	sudo cp parser4.cgi /usr/lib/cgi-bin/

webInterface3: webInterface3.ml
	$(OCAMLOPT) -o parser5.cgi $(OCAMLOPTFLAGS) $^
	sudo cp parser5.cgi /usr/lib/cgi-bin/

apiInterface: ENIAMexecTypes.ml ENIAMvisualization.ml apiInterface.ml
	$(OCAMLOPT) -o parser.api $(OCAMLOPTFLAGS) $^

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a parser test mergertest semparser parser3.cgi parser.api
