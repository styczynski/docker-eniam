This program parses the 1-million word subcorpus of the National Corpus of Polish.
The parser requires the corpus to be placed in a directory named fullCorpus located in the parser's main directory.
It also requires Python 3 as well as the OCaml compiler to be installed.

To perform the parsing run
./parse.sh
WARNING: this may take a long time.

The preprocessing and postprocessing required by the parser can be performed separately using
cd preProcessing; ./preProcess.sh
and
cd postProcessing; ./postProcess.sh
respectively.

The results of the parsing can be validated using
./validate.sh

The results of the parsing can be cleaned using
./clean.sh
