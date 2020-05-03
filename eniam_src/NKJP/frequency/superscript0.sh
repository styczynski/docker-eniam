#!/bin/bash

if [ $# -lt 1 ]
then
  echo 'Please provide an output file.'
  exit 1
fi

ghc frequency.hs
echo "Compilation complete."

ALL=$(find . -name 'ann_morphosyntax.xml' | wc -l)
COUNTER=1
for f in $(find . -name 'ann_morphosyntax.xml')
do
  echo -ne "\r\e[K$COUNTER/$ALL $f"
  if [ ! -f $f.list ]; then
    ./frequency < $f > $f.list
  fi
  let COUNTER=COUNTER+1
done
echo
echo "Processing complete."

> "$1"
for f in $(find . -name 'ann_morphosyntax.xml.list')
do
  cat $f >> "$1"
  rm $f
done
echo "Merging complete."

sed -i "/ERROR :                :interp/c\:     :       interp" "$1"
sed -i "/ERROR :)               ):subst:sg:nom:n/c\:)   :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :)               -):subst:sg:nom:n/c\:)  :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :(               (:subst:sg:nom:n/c\:(   :(      subst:sg:nom:n" "$1"
sed -i "/ERROR :(               -(:subst:sg:nom:n/c\:(  :-(     subst:sg:nom:n" "$1"
sed -i "/ERROR :\\\             \\\:subst:sg:nom:n/c\:\\\       :\\\    subst:sg:nom:n" "$1"
sed -i "/ERROR :(((             (:subst:sg:nom:n/c\:((( :(      subst:sg:nom:n" "$1"
sed -i "/ERROR :o)              o):subst:sg:nom:n/c\:o) :o)     subst:sg:nom:n" "$1"
sed -i "/ERROR :o))             o):subst:sg:nom:n/c\:o))        :o)     subst:sg:nom:n" "$1"
sed -i "/ERROR :o)              -):subst:sg:nom:n/c\:o) :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :o))))           o):subst:sg:nom:n/c\:o))))      :o)     subst:sg:nom:n" "$1"
sed -i "/ERROR :O)              o):subst:sg:nom:n/c\:O) :o)     subst:sg:nom:n" "$1"
sed -i "/ERROR :O(              o(:subst:sg:nom:n/c\:O( :o(     subst:sg:nom:n" "$1"
sed -i "/ERROR :P               P:subst:sg:nom:n/c\:P   :P      subst:sg:nom:n" "$1"
sed -i "/ERROR :P))))))))               P):subst:sg:nom:n/c\:P))))))))  :P)     subst:sg:nom:n" "$1"
sed -i "/ERROR :))              ):subst:sg:nom:n/c\:))  :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :)))             ):subst:sg:nom:n/c\:))) :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :)))))))         ):subst:sg:nom:n/c\:)))))))     :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :)))))))))               ):subst:sg:nom:n/c\:)))))))))   :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :)))))))))))))))))))             ):subst:sg:nom:n/c\:))))))))))))))))))) :)      subst:sg:nom:n" "$1"
sed -i "/ERROR :-)              -):subst:sg:nom:n/c\:-) :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :-)              -):interp/c\:-) :-)     interp" "$1"
sed -i "/ERROR :-))             -):subst:sg:nom:n/c\:-))        :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :-)))            -):subst:sg:nom:n/c\:-)))       :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :-))))           -):subst:sg:nom:n/c\:-))))      :-)     subst:sg:nom:n" "$1"
sed -i "/ERROR :-(              -(:subst:sg:nom:n/c\:-( :-(     subst:sg:nom:n" "$1"
sed -i "/ERROR :-\/             -\/:subst:sg:nom:n/c\:-\/       :-\/    subst:sg:nom:n" "$1"
sed -i "/ERROR :-D              -D:subst:sg:nom:n/c\:-D :-D     subst:sg:nom:n" "$1"
sed -i "/ERROR :O               O:subst:sg:nom:n/c\:O   :O      subst:sg:nom:n" "$1"
sed -i "/ERROR :D               D:subst:sg:nom:n/c\:D   :D      subst:sg:nom:n" "$1"
sed -i "/ERROR :|               |:subst:sg:nom:n/c\:|   :|      subst:sg:nom:n" "$1"
sed -i "/ERROR :]               ]:subst:sg:nom:n/c\:]   :]      subst:sg:nom:n" "$1"
sed -i "/ERROR (:       (       :subst:sg:nom:n/c\(:    (:      subst:sg:nom:n" "$1"
