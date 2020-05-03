#!/bin/bash

INDEXFILE=/mnt/data/korpusy/kpwr-1.1_wsd_+_disamb/index_wsd_pelnasciezka.txt 

ok=0
nook=0

while read line           
do
  if [ -f ${line} ]
  then
    ok=$((ok+1))
  else
    nook=$((nook+1))
  fi
done < ${INDEXFILE}

echo "Done:" ${ok}
echo "Todo:" ${nook}
