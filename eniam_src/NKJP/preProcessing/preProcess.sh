#!/bin/bash
ALL=$(find ../fullCorpus -maxdepth 1 -mindepth 1 -type d | wc -l)
COUNTER=1

# Supplements the morphosyntax and senses files with additional segmentation information necessary for parsing
for d in $(find ../fullCorpus -maxdepth 1 -mindepth 1 -type d)
do
  echo -ne "\r\e[K$COUNTER/$ALL $d"

  if [ ! -f $d/ann_morphosyntax_raw.xml ]; then
    mv $d/ann_morphosyntax.xml $d/ann_morphosyntax_raw.xml
    python3 preProcess.py $d/ann_morphosyntax_raw.xml $d/ann_segmentation.xml > $d/ann_morphosyntax.xml
  fi
  if [ ! -f $d/ann_senses_raw.xml ]; then
    mv $d/ann_senses.xml $d/ann_senses_raw.xml
    python3 preProcess.py $d/ann_senses_raw.xml $d/ann_segmentation.xml > $d/ann_senses.xml
  fi

  let COUNTER=COUNTER+1
done

# Fixes the error where some headers contain unescaped '>'
ALL=$(find .. -name 'header.xml' | wc -l)
COUNTER=1
for f in $(find .. -name 'header.xml')
do
  echo -ne "\r\e[K$COUNTER/$ALL $f"

  DIR=$(dirname $f)
  if [ ! -f $DIR/header_raw.xml ]; then
    mv $DIR/header.xml $DIR/header_raw.xml
    python3 fixHeader.py $DIR/header_raw.xml > $DIR/header.xml
  fi

  let COUNTER=COUNTER+1
done

# Fixes the error where some files have missing divisions
mv ../fullCorpus/310-2-000000042/text.xml ../fullCorpus/310-2-000000042/text_raw.xml
python3 fixMissingDiv.py ../fullCorpus/310-2-000000042/text_raw.xml > ../fullCorpus/310-2-000000042/text.xml
mv ../fullCorpus/310-2-000000042/ann_segmentation.xml ../fullCorpus/310-2-000000042/ann_segmentation_raw.xml
python3 fixMissingDiv.py ../fullCorpus/310-2-000000042/ann_segmentation_raw.xml > ../fullCorpus/310-2-000000042/ann_segmentation.xml
mv ../fullCorpus/310-2-000000042/ann_morphosyntax.xml tmp.xml
python3 fixMissingDiv.py tmp.xml > ../fullCorpus/310-2-000000042/ann_morphosyntax.xml
mv ../fullCorpus/310-2-000000042/ann_senses.xml tmp.xml
python3 fixMissingDiv.py tmp.xml > ../fullCorpus/310-2-000000042/ann_senses.xml
rm tmp.xml

# Fixes the error where some files use non-monotonic numbering
for d in "200-4-000000308" "200-4-000000313" "200-4-000000303"
do
  cp ../fullCorpus/$d/text.xml ../fullCorpus/$d/text_raw.xml
  cp ../fullCorpus/$d/ann_segmentation.xml ../fullCorpus/$d/ann_segmentation_raw.xml
done
for f in "text.xml" "ann_segmentation.xml" "ann_morphosyntax.xml" "ann_senses.xml"
do
  sed -i -e 's/14\.3-ab/14\.4-ab/g' ../fullCorpus/200-4-000000308/$f
  sed -i -e 's/15\.3-ab/14\.3-ab/g' ../fullCorpus/200-4-000000308/$f
done
for f in "text.xml" "ann_segmentation.xml" "ann_morphosyntax.xml" "ann_senses.xml"
do
  sed -i -e 's/10\.2-ab/10\.3-ab/g' ../fullCorpus/200-4-000000313/$f
  sed -i -e 's/12\.2-ab/10\.2-ab/g' ../fullCorpus/200-4-000000313/$f
done
for f in "text.xml" "ann_segmentation.xml" "ann_morphosyntax.xml" "ann_senses.xml"
do
  sed -i -e 's/9\.1-ab/9\.2-ab/g' ../fullCorpus/200-4-000000303/$f
  sed -i -e 's/11\.1-ab/9\.1-ab/g' ../fullCorpus/200-4-000000303/$f
done

echo
