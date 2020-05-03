#!/bin/bash
ALL=$(find ../fullCorpus -name 'fold_morph.txt' | wc -l)
COUNTER=1

for f in $(find ../fullCorpus -name 'fold_morph.txt')
do
  echo -ne "\r\e[K$COUNTER/$ALL $f"

  if [[ $(python3 checkTokens.py $f) ]]; then
    echo $f
  fi

  let COUNTER=COUNTER+1
done
echo
