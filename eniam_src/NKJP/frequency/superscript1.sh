#!/bin/bash

if [ $# -lt 2 ]
then
  echo 'Please provide an input and an output file.'
  exit 1
fi

export LC_COLLATE=C
sort "$1" | uniq --count | sed -E 's/( )*([0-9]+) (.*)/\3	\2/' > "$2"
