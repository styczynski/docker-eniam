#!/bin/bash

lexicon_input=$1
if [[ "$lexicon_input" != "empty" ]]; then
  echo $lexicon_input | base64 -d > ./data/lexicon.dic
fi
shift

velence_input=$1
if [[ "$velence_input" != "empty" ]]; then
  echo $velence_input | base64 -d > ./data/valence.dic
fi
shift

mwe_input=$1
if [[ "$mwe_input" != "empty" ]]; then
  echo $mwe_input | base64 -d > ./data/mwe.tab
fi
shift

ne_input=$1
if [[ "$ne_input" != "empty" ]]; then
  echo $ne_input | base64 -d > ./data/ne.tab
fi
shift

mkdir results 2> /dev/null > /dev/null
touch results/parsed_text.html
./domparser --output results/ $@
v=$?
if [ "$v" != "0" ]; then
  exit $v
fi

cat results/parsed_text.html
v=$?
if [ "$v" != "0" ]; then
  exit $v
fi

shopt -s nullglob
for file in ./results/*.pdf
do
	echo "@begin_pdf@: ${file}"
	cat $file
	echo "@end_pdf@"
done

shopt -s nullglob
for file in ./*.pdf
do
	echo "@begin_pdf@: ${file}"
	cat $file
	echo "@end_pdf@"
done

shopt -s nullglob
for file in ./results/*.png
do
	echo "@begin_png@: ${file}"
	cat $file
	echo "@end_png@"
done

shopt -s nullglob
for file in ./*.png
do
	echo "@begin_png@: ${file}"
	cat $file
	echo "@end_png@"
done

echo "@done@"