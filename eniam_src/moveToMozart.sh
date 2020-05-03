#!/bin/bash

cd integration
make clean
sudo make install
make clean
cd ..

cd exec
make clean
sudo make install
make clean
make parser
scp parser wjaworski@mozart.ipipan.waw.pl:~/Dokumenty/ENIAM3/exec
cd ..
