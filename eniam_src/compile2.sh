#!/bin/bash

cd lexSemantics
make clean
sudo make install
make clean
cd ..

cd semantics
make clean
sudo make install
make clean
cd ..

cd exec
make clean
sudo make install
make clean
cd ..

cd theories
sudo make install
cd ..
