#!/bin/bash

for i in `ls results/*prec*`; do echo ${i} && wosedon-gen-prec -p ${i}; done
