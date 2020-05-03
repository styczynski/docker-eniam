OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +extlib -I +xml-light -I +gsl -I +xlib -I +zip -I +bz2
OCAMLFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa

SOURCES=relation.ml ENIAMplWordnetTypes.ml ENIAMplWordnet.ml

all: $(SOURCES) ENIAMplWordnetGenerate.ml
	mkdir -p resources
	mkdir -p results
	$(OCAMLOPT) -o converter $(OCAMLOPTFLAGS) $^

analyze: $(SOURCES) ENIAMplWordnetAnalyze.ml
	mkdir -p results/rels
	$(OCAMLOPT) -o analyze $(OCAMLOPTFLAGS) $^

install:
	mkdir -p /usr/share/eniam/plWordnet
	cp resources/*  /usr/share/eniam/plWordnet

install-local:
	mkdir -p /usr/local/share/eniam/plWordnet
	cp resources/*  /usr/local/share/eniam/plWordnet

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

xlib.cmxa:
	cd xlib; make $@

xlib.cma:
	cd xlib; make $@

clean:
	rm -f *~ *.cm[oix] *.o analyze converter resources/* results/*
