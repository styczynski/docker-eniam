OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-morphology.cmxa eniam-tokenizer.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAMtokenizerTypes.ml ENIAMtagset.ml ENIAMurl.ml ENIAMtokens.ml ENIAMacronyms.ml ENIAMpatterns.ml ENIAMtokenizer.ml

all: eniam-tokenizer.cma eniam-tokenizer.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-tokenizer.cmxa eniam-tokenizer.a eniam-tokenizer.cma $(INSTALLDIR)
	cp ENIAMtokenizerTypes.cmi ENIAMtagset.cmi ENIAMurl.cmi ENIAMtokens.cmi ENIAMacronyms.cmi ENIAMpatterns.cmi ENIAMtokenizer.cmi $(INSTALLDIR)
	cp ENIAMtokenizerTypes.cmx ENIAMtagset.cmx ENIAMurl.cmx ENIAMtokens.cmx ENIAMacronyms.cmx ENIAMpatterns.cmx ENIAMtokenizer.cmx $(INSTALLDIR)
	mkdir -p /usr/share/eniam/tokenizer
	cp resources/*  /usr/share/eniam/tokenizer

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-tokenizer.cmxa eniam-tokenizer.a eniam-tokenizer.cma $(INSTALLDIR)
	cp ENIAMtokenizerTypes.cmi ENIAMtagset.cmi ENIAMurl.cmi ENIAMtokens.cmi ENIAMacronyms.cmi ENIAMpatterns.cmi ENIAMtokenizer.cmi $(INSTALLDIR)
	cp ENIAMtokenizerTypes.cmx ENIAMtagset.cmx ENIAMurl.cmx ENIAMtokens.cmx ENIAMacronyms.cmx ENIAMpatterns.cmx ENIAMtokenizer.cmx $(INSTALLDIR)
	mkdir -p /usr/local/share/eniam/tokenizer
	cp resources/mte_20151215.tab  /usr/local/share/eniam/tokenizer/mte_20151215.tab
	cp resources/top-level-domains.tab  /usr/local/share/eniam/tokenizer/top-level-domains.tab
	cp resources/README  /usr/local/share/eniam/tokenizer/README
#	ln -s /usr/local/share/eniam/tokenizer/mte_20151215.tab /usr/local/share/eniam/tokenizer/mte.tab

eniam-tokenizer.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-tokenizer.cma $(OCAMLFLAGS) $^

eniam-tokenizer.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-tokenizer.cmxa $(INCLUDES) $^

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
