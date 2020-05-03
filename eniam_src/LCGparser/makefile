OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I +eniam
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa eniam-lcg-parser.cmxa
INSTALLDIR=`ocamlc -where`/eniam

SOURCES= ENIAM_LCGtypes.ml ENIAM_LCGstringOf.ml ENIAM_LCGrules.ml ENIAM_LCGrenderer.ml ENIAM_LCGchart.ml ENIAM_LCGlatexOf.ml ENIAM_LCGreductions.ml ENIAM_LCGgraphOf.ml ENIAM_LCG_XMLof.ml

all: eniam-lcg-parser.cma eniam-lcg-parser.cmxa

install: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-parser.cmxa eniam-lcg-parser.a eniam-lcg-parser.cma $(INSTALLDIR)
	cp ENIAM_LCGtypes.cmi ENIAM_LCGstringOf.cmi ENIAM_LCGrules.cmi ENIAM_LCGrenderer.cmi ENIAM_LCGchart.cmi ENIAM_LCGlatexOf.cmi ENIAM_LCGreductions.cmi ENIAM_LCGgraphOf.cmi ENIAM_LCG_XMLof.cmi $(INSTALLDIR)
	cp ENIAM_LCGtypes.cmx ENIAM_LCGstringOf.cmx ENIAM_LCGrules.cmx ENIAM_LCGrenderer.cmx ENIAM_LCGchart.cmx ENIAM_LCGlatexOf.cmx ENIAM_LCGreductions.cmx ENIAM_LCGgraphOf.cmx ENIAM_LCG_XMLof.cmx $(INSTALLDIR)

install-local: all
	mkdir -p $(INSTALLDIR)
	cp eniam-lcg-parser.cmxa eniam-lcg-parser.a eniam-lcg-parser.cma $(INSTALLDIR)
	cp ENIAM_LCGtypes.cmi ENIAM_LCGstringOf.cmi ENIAM_LCGrules.cmi ENIAM_LCGrenderer.cmi ENIAM_LCGchart.cmi ENIAM_LCGlatexOf.cmi ENIAM_LCGreductions.cmi ENIAM_LCGgraphOf.cmi ENIAM_LCG_XMLof.cmi $(INSTALLDIR)
	cp ENIAM_LCGtypes.cmx ENIAM_LCGstringOf.cmx ENIAM_LCGrules.cmx ENIAM_LCGrenderer.cmx ENIAM_LCGchart.cmx ENIAM_LCGlatexOf.cmx ENIAM_LCGreductions.cmx ENIAM_LCGgraphOf.cmx ENIAM_LCG_XMLof.cmx $(INSTALLDIR)

eniam-lcg-parser.cma: $(SOURCES)
	ocamlc -linkall -a -o eniam-lcg-parser.cma $(OCAMLFLAGS) $^

eniam-lcg-parser.cmxa: $(SOURCES)
	ocamlopt -linkall -a -o eniam-lcg-parser.cmxa $(INCLUDES) $^

test: test.ml
	mkdir -p results
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
