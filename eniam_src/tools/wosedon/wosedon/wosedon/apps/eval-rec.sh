#!/bin/bash

EVAL_PREC=(10 20 30 40 50 60 70 80 90 100)
INDEX_FILE=/mnt/data/korpusy/kpwr-1.1_wsd_+_disamb_GTPersPRNormItModVRankNorm/index_wsd_pelnasciezka.txt
FILENAME_PREFIX=GTPersPRNormItModVRankNorm-rankperc

for pe in ${EVAL_PREC[@]}
do
  echo "Running wosed-eval for percentage of ranking: ${pe}..."
  wosedon-eval \
    -d /home/pkedzia/repos/grafon/cfg/pkedzia-localhost.db \
    -i ${INDEX_FILE} \
    -r ${FILENAME_PREFIX}-${pe}-res.csv \
    -p ${FILENAME_PREFIX}-${pe}-prec.csv \
    -rc ${FILENAME_PREFIX}-${pe}-rec.csv \
    -pr ${pe} \
    -t nkjp 2> /dev/null

  echo "Running prec-general for percentage of ranking: ${pe}..."
  python ../prec-general.py \
    -p ${FILENAME_PREFIX}-${pe}-prec.csv \
    > ${FILENAME_PREFIX}-${pe}-prec-general.csv 
done
