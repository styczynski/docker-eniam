# -*- encoding: utf-8 -*-

__author__ = 'nika'

from tree import Tree
from .node import Node
import sys, pickle, os
from xml.sax import make_parser, handler

class TreeParser(handler.ContentHandler):

    def __init__(self, path):
        self.tree = Tree(path)
        #self.current_node = None
        #self.current_tag = ""
        #self.nodes_probs = {}
        #self.nodes = {}
        #self.nodes_from_to = {}
        #self.productions = []
        #self.in_chosen_node = False
        #self.nid = None
        #self.in_chosen_children = False
        #self.current_prod = []
        #self.full_tree = False
        #self.centre = None
        self.children = []
        #self.od = None
        #self.do = None
        #self.chosen_tree = []
        #self.chosen_tree_dep = []
        #self.prob_tree = []
        #self.prob_tree_dep = []
        #self.precision = None
        #self.precision_dep = None
        #self.recall = None
        #self.recall_dep = None
        #self.chosen_tree_centres = {}
        #self.prob_tree_centres = {}
        #self.nodes_to_expand = []
        #self.amb_count = 0
        #self.is_amb = False
        #self.more_centres = []
        #self.current_arg = None
        #self.n_more_c = {}
        self.nodes_ext = {}
        self.current_prod = []
        self.in_terminal = False
        self.current_terminal = {}
        self.current_terminal_id = None

    def getTree(self):
            return self.tree

    def startElement(self, name, attrs):
        self.current_tag = name
        if name == "node":
            attr_dict = {}
            for name in attrs.getNames():
                attr_dict[name] = attrs.getValue(name)
            self.current_node = Node(attr_dict)
            if "chosen" in attrs.getNames() and attrs.getValue("chosen") == "true":
                self.tree.addChosenNode(self.current_node)
            else:
                self.tree.addNode(self.current_node)
        elif name == "children":
            pass
        elif name == "child":
            if attrs.getValue("head") == "true":
                self.centre = attrs.getValue("nid")
                self.tree.addHead(self.current_node.getID(), self.centre)
            else:
                self.children.append(attrs.getValue("nid"))
            self.current_prod.append(( attrs.getValue("nid"), attrs.getValue("head") ))
        elif name == "terminal":
            self.current_node.setTerminal()
            self.current_terminal = {}
            self.in_terminal = True
            self.current_terminal['id'] = int(self.current_node.getFrom())
            self.tree.addHead(self.current_node.getID(), self.current_node.getID())
        elif name == "nonterminal":
            self.current_node.setNonterminal()
        elif name == "f":
            self.current_arg = attrs.getValue("type")

    def endElement(self, name):                         # zlicza w zależności od węzła
        if name == "node":
            self.productions = []
            self.in_chosen_node = False
        if name == "children":
            if self.centre:
                self.current_node.addChildren( {'centre': self.centre, 'productions':self.children, 'whole_prod':self.current_prod} )
            else:
                self.current_node.addChildren( {'productions':self.children, 'whole_prod':self.current_prod} )
                """
                if len(self.children) == 1:
                    self.current_node.addChildren( {'centre': self.children[0], 'productions':[], 'whole_prod':self.current_prod} )
                else:
                    print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
                """
            self.tree.addParent(self.current_node, self.children + [self.centre])
            self.children = []
            self.in_chosen_children = False
            self.centre = None
            self.current_prod = []
        if name == "terminal":
            self.tree.addLeaf(self.current_terminal['id'], self.current_terminal)
            self.in_terminal=False

    def characters(self, content):
        if self.current_tag == "category" and content.strip():
            self.current_node.setCategory( content.strip() )
        if self.current_tag == "f" and content.strip():
            if self.current_arg == "tag" and self.in_terminal:
                self.current_terminal['tag'] = content.strip()
            else:
                self.current_node.addArgument(self.current_arg, content.strip())
        if self.current_tag == "orth" and self.in_terminal and content.strip():
            self.current_terminal["orth"] = content.strip()
        if self.current_tag == "base" and self.in_terminal and content.strip():
            self.current_terminal["base"] = content.strip()
            
    def endDocument(self):
        pass#self.tree.findHeads()

    """
    def count_probs(self, node_id):
        if node_id in self.nodes_probs:
            return self.nodes_probs[node_id]
        try:
            poss_prods = self.tree[node_id]
        except KeyError:
            poss_prods = []
        if not poss_prods:
            return 1.0
        for chosen, prod_dict in poss_p
            if prod_dict['productions']:
                prod_dict['prob'] = 1.0
                for prod in prod_dict['productions']:
                    try:
                        prod_dict['prob'] *= self.count_probs(prod) * self.count_probs(prod_dict['centre']) *\
                                             grammar[(self.nodes[node_id],\
                                                     self.nodes[prod])]
                    except KeyError:
                        prod_dict['prob'] = 0.0
            else:
                try:
                    prod_dict['prob'] = 1.0 *\
                                        self.count_probs(prod_dict['centre']) *\
                                        grammar[(self.nodes[node_id], self.nodes[prod_dict['centre']], None)]
                except KeyError:
                    prod_dict['prob'] = 0.0
        self.nodes_probs[node_id] = sorted(poss_prods, key=lambda x: x[1]['prob'])[0][1]['prob']
        return self.nodes_probs[node_id]



    def get_prob_tree(self, node_id):
        poss_prods = self.tree[node_id]
        if poss_prods:
            try:
                prod_dict = sorted(poss_prods, key=lambda x: x[1]['prob'])[0][1]
            except KeyError:
                prod_dict = poss_prods[0][1]
            self.prob_tree.append(( node_id, self.nodes_from_to[node_id] ))#self.prob_tree.append(( self.nodes[node_id], self.nodes_from_to[node_id] ))
            if prod_dict['centre']:
                self.get_prob_tree(prod_dict['centre'])
            for prod in prod_dict['productions']:
                self.prob_tree_dep.append((node_id, prod))
                self.get_prob_tree(prod)
            if self.nodes_from_to[node_id][1] - self.nodes_from_to[node_id][0] == 1:
                self.prob_tree_centres[node_id] = self.nodes_from_to[node_id][0]
            else:
                self.prob_tree_centres[node_id] = self.prob_tree_centres[prod_dict['centre']]
        else:
            if self.nodes_from_to[node_id][1] - self.nodes_from_to[node_id][0] == 1:
                self.prob_tree_centres[node_id] = self.nodes_from_to[node_id][0]

    def endDocument(self):
        print self.amb_count
        if u'0' in self.nodes:
            self.count_probs(u'0')
            self.get_chosen_tree(u'0')
            self.get_prob_tree(u'0')
            #nodes_to_expand = map(lambda x: x[0], self.prob_tree_dep)
            #while self.nodes_to_expand:
            #for ii in self.nodes_to_expand:
            #print self.nodes_to_expand, sorted(self.prob_tree_centres.keys(), key = lambda x: int(x))
            #    if ii[1] in self.prob_tree_centres:
            #        self.prob_tree_centres[ii[0]] = self.prob_tree_centres[ii[1]]
            #        self.nodes_to_expand.remove(ii)
            #print self.nodes_from_to
            self.precision = 1.0 * len(list((set(self.chosen_tree).intersection(set(self.prob_tree))))) / len(self.chosen_tree)
            self.recall = 1.0 * len(list((set(self.chosen_tree).intersection(set(self.prob_tree))))) / len(self.chosen_tree)
            #print self.precision, self.recall
            #print sorted(list(set(self.chosen_tree).difference(set(self.prob_tree))), key=lambda x: x[1])
            #print sorted(list(set(self.prob_tree).difference(set(self.chosen_tree))), key=lambda x: x[1])
            #print self.chosen_tree_centres

            self.chosen_tree_dep = map(lambda x: (self.chosen_tree_centres[x[1]], self.chosen_tree_centres[x[0]]), self.chosen_tree_dep)
            self.prob_tree_dep = map(lambda x: (self.prob_tree_centres[x[1]], self.prob_tree_centres[x[0]]), self.prob_tree_dep)
            for ii in range(len(self.chosen_tree_dep)+1):
                if ii not in map(lambda x: x[0], self.chosen_tree_dep):
                    self.chosen_tree_dep.append((ii, "root"))
            for ii in range(len(self.prob_tree_dep)+1):
                if ii not in map(lambda x: x[0], self.prob_tree_dep):
                    self.prob_tree_dep.append((ii, "root"))
                #print self.chosen_tree_dep, self.prob_tree_dep
            self.precision_dep = 1.0 * len(list(set(self.chosen_tree_dep).intersection( set(self.prob_tree_dep)))) / len(self.prob_tree_dep)
    """

