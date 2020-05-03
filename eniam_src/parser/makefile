OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
INCLUDES=-I +xml-light -I +xlib -I +zip -I +bz2 -I ../../../Dropbox/lib/latexvis -I ../../installed/latexvis -I ../lib/xt -I ../../../Dropbox/Clarin-pl/podzadania/nkjp/fold_text -I ../podzadania/morfeusz -I ../pre -I ../corpora -I ../diagnostics -I ../tokenizer -I ../subsyntax -I ../walenty -I ../lexSemantics -I ../integration
#INCLUDES=-I +xml-light -I +xlib -I ../pre
OCAMLFLAGS=$(INCLUDES) -g
OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa zip.cmxa bz2.cmxa xlib.cmxa latexvis.cmxa #nkjp.cmxa
#OCAMLOPTFLAGS=$(INCLUDES) unix.cmxa xml-light.cmxa str.cmxa nums.cmxa xlib.cmxa

PRE= ../pre/paths.ml ../tokenizer/ENIAMtokenizerTypes.ml ../subsyntax/ENIAMsubsyntaxTypes.ml ../walenty/ENIAMwalTypes.ml ../lexSemantics/ENIAMlexSemanticsTypes.ml ../walenty/ENIAMwalStringOf.ml ../integration/ENIAM_CONLL.ml
LCG= LCGtypes.ml LCGstringOf.ml LCGrules.ml LCGrenderer.ml LCGchart.ml LCGlatexOf.ml LCGreductions.ml LCGlexicon2.ml LCGlexicon.ml LCGvalence.ml
#LCG= LCGtypes.ml LCGstringOf.ml LCGrules.ml LCGrenderer.ml LCGchart.ml LCGreductions.ml LCGlexicon.ml LCGvalence.ml
DISAMB= disambSelPref.ml disambLemma.ml
SEM= semGraph.ml semTypes.ml semStringOf.ml semLatexOf.ml semMmlOf.ml semMrl.ml
#SEM= semGraph.ml semTypes.ml semStringOf.ml semMmlOf.ml semMrl.ml
EXEC= execTypes.ml visualization.ml exec.ml

all:
	$(OCAMLOPT) -o pipe $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) pipe.ml
#	$(OCAMLOPT) -o server2 $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) server.ml
#	$(OCAMLOPT) -o parser2.cgi $(OCAMLOPTFLAGS) $(PRE) LCGtypes.ml LCGstringOf.ml LCGrules.ml LCGrenderer.ml LCGchart.ml  LCGlatexOf.ml semTypes.ml semMmlOf.ml execTypes.ml visualization.ml webInterface.ml
# $(OCAMLOPT) -o eniam.distr $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) overseer.ml
# $(OCAMLOPT) -o eniam.worker $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) worker.ml
#	$(OCAMLOPT) -o parser.api $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) apiInterface.ml
# $(OCAMLOPT) -o eniam $(OCAMLOPTFLAGS) $(PRE) $(LCG) $(DISAMB) $(SEM) $(EXEC) singleInterface.ml

# pipe:
# 	$(OCAMLOPT) -o pipe $(OCAMLOPTFLAGS) $(PRE) $(PROC_F)
#
# of_xml:
# 	$(OCAMLOPT) -o of_xml $(OCAMLOPTFLAGS) LCGofXml.ml

swigra_test: swigra_test.ml
	$(OCAMLOPT) -o swigra_test $(OCAMLOPTFLAGS) swigra_test.ml


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
	rm -f *~ *.cm[oix] *.o eniam eniam.distr eniam.worker server2 parser2.cgi pipe
