#!/bin/bash

# Undoes the changes in numbering made in order to fix cases of missing divisions
mv ../fullCorpus/310-2-000000042/fold_text.txt tmp.txt
python3 unfixMissingDiv.py tmp.txt 1 > ../fullCorpus/310-2-000000042/fold_text.txt
for l in "segm" "morph" "sense"
do
  mv ../fullCorpus/310-2-000000042/fold_$l.txt tmp.txt
  python3 unfixMissingDiv.py tmp.txt 0 > ../fullCorpus/310-2-000000042/fold_$l.txt
done
rm tmp.txt

# Undoes the changes in numbering made in order to fix cases of non-monotonic numbering
for f in "fold_text.txt" "fold_segm.txt" "fold_morph.txt" "fold_sense.txt"
do
  sed -i -e 's/14\.3-ab/15\.3-ab/g' ../fullCorpus/200-4-000000308/$f
  sed -i -e 's/14\.4-ab/14\.3-ab/g' ../fullCorpus/200-4-000000308/$f
done
for f in "fold_text.txt" "fold_segm.txt" "fold_morph.txt" "fold_sense.txt"
do
  sed -i -e 's/10\.2-ab/12\.2-ab/g' ../fullCorpus/200-4-000000313/$f
  sed -i -e 's/10\.3-ab/10\.2-ab/g' ../fullCorpus/200-4-000000313/$f
done
for f in "fold_text.txt" "fold_segm.txt" "fold_morph.txt" "fold_sense.txt"
do
  sed -i -e 's/9\.1-ab/11\.1-ab/g' ../fullCorpus/200-4-000000303/$f
  sed -i -e 's/9\.2-ab/9\.1-ab/g' ../fullCorpus/200-4-000000303/$f
done

# Fixes the discrepancies in forms containing typos between text and ann_morphosyntax
./fixForms.sh

# Fixes form errors
sed -i -e 's/4746\.466104-seg; 26; 3; c.o;/4746\.466104-seg; 26; 4; c.o.;/g' ../fullCorpus/130-3-900001/fold_morph.txt
