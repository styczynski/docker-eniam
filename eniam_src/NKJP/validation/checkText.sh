#!/bin/bash
ALL=$(find ../fullCorpus -maxdepth 1 -mindepth 1 -type d | wc -l)
COUNTER=1

for f in $(find ../fullCorpus -maxdepth 1 -mindepth 1 -type d)
do
  echo -ne "\r\e[K$COUNTER/$ALL $f"

  python3 checkText.py $f
  if [[ $(diff --from-file extract_text.txt extract_fold_text.txt extract_fold_segm.txt extract_fold_morph.txt) ]]; then
    echo $f
  elif [[ $(python3 subfile.py extract_fold_sense.txt extract_fold_text.txt) != "True" ]]; then
    echo $f
  fi

  let COUNTER=COUNTER+1
done
rm extract_*
echo
