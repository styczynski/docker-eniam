#!/bin/bash
ALL=$(find ../fullCorpus -name 'fold_morph.txt' | wc -l)
COUNTER=1

for f in $(find ../fullCorpus -name 'fold_morph.txt')
do
  echo -ne "\r\e[K$COUNTER/$ALL $f"

  cp $f tmp.txt
  python3 fixForms.py tmp.txt > $f

  let COUNTER=COUNTER+1
done

rm tmp.txt
echo
