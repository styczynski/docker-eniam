#!/bin/bash
ALL=$(find fullCorpus -maxdepth 1 -mindepth 1 -type d | wc -l)
COUNTER=1
DIR=data/NKJP-PodkorpusMilionowy-1.2

echo "Preprocessing:"
cd preProcessing
./preProcess.sh
cd ..
echo "Preprocessing done."

make

echo "Parsing:"
# Performs the parsing
for d in $(find fullCorpus -maxdepth 1 -mindepth 1 -type d)
do
  echo -ne "\r\e[K$COUNTER/$ALL $d"

  if [ ! -f $d/fold_text.txt ]; then
    mv $d $DIR
    ./test > debug.txt
    mv $DIR/$(basename $d) fullCorpus
    mv $DIR/fold_text.txt $DIR/fold_segm.txt $DIR/fold_morph.txt $DIR/fold_sense.txt $d
  fi

  let COUNTER=COUNTER+1
done
echo
echo "Parsing done."

echo "Postprocessing:"
cd postProcessing
./postProcess.sh
cd ..
echo "Postprocessing done."
