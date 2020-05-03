#!/bin/bash

while true 
do
  find results/web/* -mtime +1 -delete
  sleep 1d
done