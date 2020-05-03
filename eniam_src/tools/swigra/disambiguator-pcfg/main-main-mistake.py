# -*- encoding: utf-8 -*-
__author__ = 'nika'

from xml.sax import make_parser
from treeparser import TreeParser
from productionparseractpcfg import makeActPcfg
from node import Node
from tree import Tree
import glob
from multiprocessing import Pool
from sklearn.cross_validation import KFold


############################# CZYTANIE WSZYSTKICH PLIKÓW

all_files = []
for f in glob.iglob("/home/nika/zrobione140523/*/*/*-s.xml"):
    all_files.append(f)
print len(all_files)
all_files = all_files
############################# WALIDACJA KRZYŻOWA

def ev(args):
    f, grammar = args
    parser = make_parser()
    handler = TreeParser(f)
    parser.setContentHandler(handler)
    parser.parse(f)
    tree = handler.getTree()
    return tree.act_pcfg(grammar), tree.countChildrenZadnie()#[]#act_pcfg(grammar), tree.findMistakesSimple()

def count_ev(list_of_tuples):
    lll = []
    for ii in range(len(list_of_tuples[0])):
        lll.append(sum(map(lambda x: x[ii], list_of_tuples))/len(list_of_tuples))
    return lll

def count_ev_repr(list_of_tuples):
    lll = []
    for ii in range(len(list_of_tuples[0][0])):
        lll.append(sum(map(lambda x: x[0][ii], list_of_tuples))/len(list_of_tuples))
    for ii in range(len(list_of_tuples[0][1])):
        lll.append(sum(map(lambda x: x[1][ii], list_of_tuples))/len(list_of_tuples))
    return lll

# self.agreed_count, self.agreed_strict_count, self.chosen_nodes_count, self.disamb_nodes_count, self.dep_agreed_count, self.dep_count
def repr_stats(list_of_stats):
    all_sum = []
    for ii in range(len(list_of_stats[0])):
        all_sum.append(sum(map(lambda x:x[ii], list_of_stats)))
    result = [[1.0*all_sum[0]/all_sum[2], 1.0*all_sum[0]/all_sum[3], 1.0*all_sum[1]/all_sum[2], 1.0*all_sum[1]/all_sum[3], 1.0*all_sum[4]/all_sum[5]]]
    tmp = map(lambda x:[1.0*x[0]/x[2], 1.0*x[0]/x[3], 1.0*x[1]/x[2], 1.0*x[1]/x[3], 1.0*x[4]/x[5]], list_of_stats)
    all_sum = []
    for ii in range(len(tmp[0])):
        all_sum.append(sum(map(lambda x:x[ii], tmp))/len(tmp))
    result.append( all_sum )
    return result




all_lex_pcfg = []
mistakes =[]
for train, test in KFold(len(all_files), 10):
    print len(train), len(test)
    train_files = map(lambda x: all_files[x], train)
    print "making grammar ..."
    grammar = makeActPcfg(train_files, 20)
    test_files = map(lambda x: (all_files[x], grammar), test)
    print "evaluating ..."#, test_files
    pool = Pool(processes=20)              # start 4 worker processes
    #for ii in test_files: ev(ii)
    results = pool.map(ev, test_files)
    all_lex_pcfg.append(repr_stats(map(lambda x: x[0],results)))
    mistakes += map(lambda x:x[1], results)
    print "***************************************************************************************"
    print "RESULT", len(all_lex_pcfg), ":          ",  all_lex_pcfg[-1]


print "***************************************************************************************"
print "RESULT:          ",   count_ev_repr(all_lex_pcfg)

"""

mistakes_dict = {}
for ii in mistakes:
    for jj in ii:
        if jj in mistakes_dict:
            mistakes_dict[jj] +=1
        else:
            mistakes_dict[jj] =1

for jj in mistakes:
    for mistake in jj.keys():
        if mistake in mistakes_dict:
            mistakes_dict[mistake] += jj[mistake]
        else:
            mistakes_dict[mistake] = jj[mistake]


import pickle

for ii in sorted(mistakes_dict, key= lambda x:-mistakes_dict[x]):
    print mistakes_dict[ii], ii

pickle.dump(mistakes_dict, open("count-zdanie-dump.pkl", "w"))
"""
