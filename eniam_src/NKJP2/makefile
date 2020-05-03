OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-tokenizer.cmxa eniam-morphology.cmxa eniam-subsyntax.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES=ENIAM_NKJP.ml validateTokenizer.ml validateMorphology.ml #validateSubsyntax.ml

all: $(SOURCES)
	$(OCAMLOPT) -o test $(OCAMLOPTFLAGS) $^

spelling: $(SOURCES) spelling.ml
	mkdir -p NKJP1M_spelling_errors
	$(OCAMLOPT) -o spelling $(OCAMLOPTFLAGS) $^

conll: ENIAM_NKJP.ml convertCONLL.ml
	$(OCAMLOPT) -o conll $(OCAMLOPTFLAGS) $^

# install:
# 	mkdir -p /usr/share/eniam/Walenty
# 	cp resources/*  /usr/share/eniam/Walenty
#
# install-local:
# 	mkdir -p /usr/local/share/eniam/Walenty
# 	cp resources/*  /usr/local/share/eniam/Walenty

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
	rm -f *~ *.cm[aoix] *.o *.so *.cmxa *.a test spelling
