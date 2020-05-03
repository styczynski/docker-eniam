#!/bin/bash

lexicon_input=$1
if [[ "$lexicon_input" != "empty" ]]; then
  echo $lexicon_input | base64 -d > ./data/lexicon.dic
fi
shift

./subsyntax $@ \
&& exit 0