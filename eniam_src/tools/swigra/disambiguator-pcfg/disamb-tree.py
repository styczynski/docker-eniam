# -*-  encoding: utf-8 -*-
__author__ = 'nika'

from xml.sax import make_parser
from treeparser import TreeParser
import os
import pickle
import sys
import xml.dom.minidom
import codecs

try:
    f = sys.argv[1]
except IndexError:
    print("ERROR: no filename given")
    exit()

# parse xml file for future processing
dom = xml.dom.minidom.parse(sys.argv[1])
nodes = dom.getElementsByTagName("node")

if nodes: # if there is anything to disambiguate
    # load grammar
#    grammar = pickle.load(open("grammars/pcfg-tfw-130718.pkl"))
    path = os.path.join(os.path.dirname(__file__), "grammars/pcfg-tfw-150326.pkl")
    grammar = pickle.load(open(path))
    # make parser, parse tree
    parser = make_parser()
    handler = TreeParser(f)
    parser.setContentHandler(handler)
    parser.parse(f)
    tree = handler.getTree()

    # disambiguation
    tree.act_pcfg(grammar)
    disamb_nodes = tree.getDisambNodes()

# update chosen nodes (if any)
for node in nodes:
    if node.attributes["nid"].value in disamb_nodes:
        node.attributes["chosen"] = "true"
        children_all = node.getElementsByTagName("children")
        for children in children_all:
            chosen = True
            for child in children.getElementsByTagName("child"):
                if child.attributes["nid"].value not in disamb_nodes:
                    chosen = False
            if chosen:
                children.attributes["chosen"] = "true"
    else:
        node.attributes["chosen"] = "false"

new_f = sys.argv[1].rsplit(".", 1)[0] + "-disamb.xml"
print "saving in :", new_f
open(new_f, 'w').write(codecs.encode(dom.toxml(), 'utf-8'))
