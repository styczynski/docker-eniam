#!/bin/bash

lexicon_input=$1
if [[ "$lexicon_input" != "empty" ]]; then
  echo $lexicon_input | base64 -d > ./data/lexicon.dic
fi
shift

mkdir results 2> /dev/null > /dev/null
touch results/parsed_text.html
./domparser --output results/ $@ \
&& cat results/parsed_text.html \
&& cat results/E_3_chart_selection.pdf \
&& exit 0