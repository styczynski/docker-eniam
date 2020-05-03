#!/bin/bash

make clean

for d in $(find fullCorpus -maxdepth 1 -mindepth 1 -type d); do
  for f in text segm morph sense; do
    if [ -f $d/fold_$f.txt ]; then
      rm $d/fold_$f.txt
    fi
  done
  for f in text ann_segmentation ann_morphosyntax ann_senses header; do
    if [ -f $d/${f}_raw.xml ]; then
      mv $d/${f}_raw.xml $d/$f.xml
    fi
  done
done

if [ -f test ]; then
  rm test
fi

if [ -f debug.txt ]; then
  rm debug.txt
fi

if [ -d __pycache__ ]; then
  rm -r __pycache__
fi
