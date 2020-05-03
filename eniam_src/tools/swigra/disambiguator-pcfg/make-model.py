# -*- encoding: utf-8 -*-
__author__ = 'nika'

from productionparseractpcfg import makeActPcfg
import glob
import pickle


############################# CZYTANIE WSZYSTKICH PLIKÓW

all_files = []
for f in glob.iglob("/home/nika/zrobione150326/*/*/*-s.xml"):
    all_files.append(f)

############################# TWORZENIE GRAMATYKI

grammar = makeActPcfg(all_files, 20) ## pliki wejściowe, liczba wątków

############################# ZRZUT DO PLIKU

pickle.dump(grammar, open("pcfg-grammar.pkl", "w"))