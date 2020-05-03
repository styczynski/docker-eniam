#!/bin/bash

PREFIX=whole
ITNUM=15
DIRECTED=indirected
# RERANKERS=LemmaNorm
RERANKERS=Vertex-LemmaNorm
ALG_MET=GTPersPRNormItModVRankNorm

wosedon-eval \
  -i /mnt/data/korpusy/kpwr-1.1_wsd_+_disamb/index_wsd_pelnasciezka.txt \
  -r ${PREFIX}-${ALG_MET}-${RERANKERS}-res-${ITNUM}-${DIRECTED}.csv \
  -rc  ${PREFIX}-${ALG_MET}-${RERANKERS}-rec-${ITNUM}-${DIRECTED}.csv \
  -p ${PREFIX}-${ALG_MET}-${RERANKERS}-prec-${ITNUM}-${DIRECTED}.csv \
  -t nkjp \
  -d /home/pkedzia/repos/grafon/cfg/pkedzia-localhost.db
