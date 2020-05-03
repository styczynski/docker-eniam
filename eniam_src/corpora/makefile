OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa xt.cmxa \
  eniam-morphology.cmxa eniam-tokenizer.cmxa eniam-subsyntax.cmxa #eniam-integration.cmxa eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa eniam-lexSemantics.cmxa eniam-semantics.cmxa eniam-exec.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= types.ml CONLL.ml resources.ml conllParser.ml interpsInCorpus.ml generate.ml

all: eniam-corpora.cma eniam-corpora.cmxa freq_test
	$(OCAMLOPT) -o generate $(OCAMLOPTFLAGS) $(SOURCES)

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-corpora.cmxa eniam-corpora.a eniam-corpora.cma $(INSTALLDIR)
	cp types.cmi CONLL.cmi resources.cmi conllParser.cmi interpsInCorpus.cmi generate.cmi $(INSTALLDIR)
	cp types.cmx CONLL.cmx resources.cmx conllParser.cmx interpsInCorpus.cmx generate.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/corpora
	cp info_sentences*  /usr/share/eniam/corpora

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-corpora.cmxa eniam-corpora.a eniam-corpora.cma $(INSTALLDIR)
	cp types.cmi CONLL.cmi resources.cmi conllParser.cmi interpsInCorpus.cmi generate.cmi $(INSTALLDIR)
	cp types.cmx CONLL.cmx resources.cmx conllParser.cmx interpsInCorpus.cmx generate.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/corpora
	cp info_sentences*  /usr/local/share/eniam/corpora

eniam-corpora.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-corpora.cma $(OCAMLFLAGS) $^

eniam-corpora.cmxa: $(SOURCES)
	$(OCAMLOPT) -linkall -a -o eniam-corpora.cmxa $(INCLUDES) $(SOURCES)

freq_test:
	$(OCAMLOPT) -o freq_test $(OCAMLOPTFLAGS) $(SOURCES) freq_test.ml

test: CONLL.ml test_conll.ml
	mkdir -p results
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) eniam-domainLexSemantics.cmxa $^

test2: CONLL2.ml validateTokenizer.ml test_conll2.ml
	mkdir -p results
	$(OCAMLOPT) -o test2 $(OCAMLOPTFLAGS) $^


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
	rm -f *~ *.cm[oix] *.o generate corpora freq_test test test2
