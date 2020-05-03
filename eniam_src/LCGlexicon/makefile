OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-tokenizer.cmxa eniam-lcg-parser.cmxa eniam-lcg-lexicon.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAM_LCGlexiconTypes.ml ENIAMcategoriesPL.ml ENIAM_LCGlexiconParser.ml ENIAM_LCGlexicon.ml

all: eniam-lcg-lexicon.cma eniam-lcg-lexicon.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-lexicon.cmxa eniam-lcg-lexicon.a eniam-lcg-lexicon.cma $(INSTALLDIR)
	cp ENIAM_LCGlexiconTypes.cmi ENIAMcategoriesPL.cmi ENIAM_LCGlexiconParser.cmi ENIAM_LCGlexicon.cmi $(INSTALLDIR)
	cp ENIAM_LCGlexiconTypes.cmx ENIAMcategoriesPL.cmx ENIAM_LCGlexiconParser.cmx ENIAM_LCGlexicon.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/LCGlexicon
	cp resources/*  /usr/share/eniam/LCGlexicon

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-lexicon.cmxa eniam-lcg-lexicon.a eniam-lcg-lexicon.cma $(INSTALLDIR)
	cp ENIAM_LCGlexiconTypes.cmi ENIAMcategoriesPL.cmi ENIAM_LCGlexiconParser.cmi ENIAM_LCGlexicon.cmi $(INSTALLDIR)
	cp ENIAM_LCGlexiconTypes.cmx ENIAMcategoriesPL.cmx ENIAM_LCGlexiconParser.cmx ENIAM_LCGlexicon.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/LCGlexicon
	cp resources/*  /usr/local/share/eniam/LCGlexicon


eniam-lcg-lexicon.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-lcg-lexicon.cma $(OCAMLFLAGS) $^

eniam-lcg-lexicon.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-lcg-lexicon.cmxa $(INCLUDES) $^

test: test.ml
	mkdir -p results
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) test.ml

print_lexicon: ENIAM_LCGlexiconLatexOf.ml
	mkdir -p results
	$(OCAMLOPT) -o print_lexicon $(OCAMLOPTFLAGS) ENIAM_LCGlexiconLatexOf.ml

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test print_lexicon
