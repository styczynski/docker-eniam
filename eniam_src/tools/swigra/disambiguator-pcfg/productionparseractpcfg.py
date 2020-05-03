# -*- encoding: utf-8 -*-
__author__ = 'nika'

from xml.sax import handler, make_parser
from multiprocessing import Pool

class ProductionParserActPCFG(handler.ContentHandler):

    def __init__(self):
        self.current_tag = ""
        self.nodes = {}
        self.productions = []
        self.productions_dict = {}
        self.in_chosen_node = False
        self.nid = None
        self.in_chosen_children = False
        self.current_prod = []
        self.full_tree = False
        self.prod_with_centre = []
        self.is_in_fw = False
        self.is_in_l = False
        self.is_in_o = False
        self.is_in_r = False
        self.is_in_p = False
        self.is_in_tfw = False

    def getProductions(self):
        for prod in self.productions_dict:
            pass#print prod, self.productions_dict[prod]
        return self.productions_dict

    def startElement(self, name, attrs):
        self.current_tag = name
        if name == "node" and attrs.getValue("chosen") == "true":
            self.in_chosen_node = True
            self.nid = attrs.getValue("nid")
        if name == "children" and "chosen" in attrs.getNames() and attrs.getValue("chosen") == "true":
            self.in_chosen_children = True
        if name == "child" and self.in_chosen_children:
            self.current_prod.append(( attrs.getValue("nid"), attrs.getValue("head") ))
        if name == "terminal" and self.in_chosen_node:
            self.nodes[self.nid] = u'terminal'
        if name == "f" and self.in_chosen_node:
            if attrs.getValue("type") == "tfw" and self.is_in_fw:
                self.is_in_tfw = True
            elif attrs.getValue("type") == "liczba":
                self.is_in_l = True
            elif attrs.getValue("type") == "osoba":
                self.is_in_o = True
            elif attrs.getValue("type") == "rodzaj":
                self.is_in_r = True
            elif attrs.getValue("type") == "przypadek":
                self.is_in_p = True


    def endElement(self, name):                         # zlicza w zależności od węzła
        if name == "node":
            self.in_chosen_node = False
        if name == "children" and self.in_chosen_children:
            self.in_chosen_children = False
            self.productions.append(( self.nid, self.current_prod ))
            self.current_prod = []

    def characters(self, content):
        if self.current_tag == "category" and self.in_chosen_node and content.strip():
            self.nodes[self.nid] = content.strip()
            if content.strip() == "fw":
                self.is_in_fw = True
        if self.current_tag == "f" and content.strip() and self.in_chosen_node:
            if self.is_in_tfw:
                self.nodes[self.nid] += '@' + content.strip()
                self.is_in_fw = False
                self.is_in_tfw = False
            elif self.is_in_l:
                #self.nodes[self.nid] += '@' + content.strip()
                self.is_in_l = False
            elif self.is_in_o:
                #self.nodes[self.nid] += '@' + content.strip()
                self.is_in_o = False
            elif self.is_in_r:
                #self.nodes[self.nid] += '@' + content.strip()
                self.is_in_r = False
            elif self.is_in_p:
                #self.nodes[self.nid] += '@' + content.strip()
                self.is_in_p = False


    def endDocument(self):
        tmp_prod = map(lambda x: (self.nodes[x[0]], map(lambda y: (self.nodes[y[0]], y[1]), x[1] )), self.productions)
        for ii in list(set(map(lambda x: (x[0],str(x[1])),tmp_prod))):
            self.productions_dict[ii] = map(lambda x: (x[0],str(x[1])),tmp_prod).count(ii)

def runParse(fname):
    parser = make_parser()
    handler = ProductionParserActPCFG()
    parser.setContentHandler(handler)
    parser.parse(fname)
    return handler.getProductions()

def makeActPcfg(flist, tasks=4):
    """

    """
    pool = Pool(processes=tasks)
    all_prods = {}
    for productions in pool.map(runParse, flist):
        for production in productions:
            if production in all_prods:
                all_prods[production] += productions[production]
            else:
                all_prods[production] = productions[production]

    non_term_count = {}
    prods_prob = {}
    #print all_prods
    non_term = map(lambda x: x[0], all_prods)
    for ii in set(non_term):
        non_term_count[ii] = 0
        for jj in all_prods:
            if jj[0] == ii:
                non_term_count[ii] += all_prods[jj]
    for ii in all_prods:
        prods_prob[ii] = all_prods[ii] * 1.0 / non_term_count[ii[0]]

    return prods_prob