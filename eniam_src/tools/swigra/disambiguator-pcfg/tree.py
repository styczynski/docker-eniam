# -*- encoding: utf-8 -*-
__author__ = 'nika'
import pickle

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

class Tree(object):
    def __init__(self, path_):
        self.leaves = {} # słownik
        self.path = path_
        self.root = None
        self.nodes = {} # słownik id:node
        self.chosen_nodes = [] # tu tylko ids
        self.disamb_nodes = []
        self.chosen_dep_nodes = {} # tutaj (id, zależność)
        self.disamb_dep_nodes = {}
        self.mistakes = []
        self.nodes_prods = {}
        self.chosen_nodes_count = 0
        self.disamb_nodes_count = 0
        self.agreed_count = 0
        self.agreed_strict_count = 0
        self.dep_count = 0
        self.dep_agreed_count = 0
        self.missed_prods = []
        self.parents = {u'0':u'0'}
        self.parents_labels = {u'0':u'0'}
        self.heads = {}

    def hasRoot(self):
        if self.root:
            return True
        else:
            return False

    def addNode(self, node):
        self.nodes[node.getID()] = node
        self.nodes_prods[node.getID()] = []
        if not self.hasRoot():
            self.root = node

    def addChosenNode(self, node):
        self.addNode(node)
        self.chosen_nodes.append(node.getID())

    def addProd(self, node, prod):
        self.nodes_prods[node.getID()].append(prod)

    def addParent(self, node, children):
        node_id = node.getID()
        for child in children:
            if not child in self.parents:
                self.parents[child] = [node_id]
            else:
                self.parents[child].append(node_id)

    def addLeaf(self,id,info):
        self.leaves[id] = info
        
    def addHead(self, me, child):
        self.heads[me] = child
        
    
    def findHeads(self):
        all_leaves = False
        while not all_leaves:
            all_leaves = True
            for ii in self.heads:
                self.heads[ii] = self.heads[self.heads[ii]]
                if not self.nodes[self.heads[ii]].isTerminal():
                    all_leaves = False
        


    ################################ DEPENDENCY TREE MAKING

    def childIsChosen(self, child, tree):
        is_chosen = True
        for nid in child['productions']:
            if nid not in tree:
                is_chosen = False
        if child['centre'] and child['centre'] not in tree:
                is_chosen = False
        return is_chosen

    def get_centres(self, tree_from, chosen_tree_centres, deps, node_id):
        node = self.nodes[node_id]
        for child in node.getAllChildren():
            if self.childIsChosen(child, tree_from):
                #print node_id, child
                if child['centre']:
                    self.get_centres(tree_from, chosen_tree_centres, deps, child['centre'])
                    chosen_tree_centres[node_id] = chosen_tree_centres[child['centre']]
                for nid in child['productions']:
                    self.get_centres(tree_from, chosen_tree_centres, deps, nid)
                    deps.append((nid, node_id))
                if node.getTo() - node.getFrom() == 1:
                    chosen_tree_centres[node_id] = node.getTo()
        if node.isTerminal() and node.getTo() - node.getFrom() == 1:
            chosen_tree_centres[node_id] = node.getTo()

    def get_dep_tree(self, tree_from):
        chosen_tree_centres = {'sID':0}                    # tu będzie nid:najbl.centrum
        deps = [(self.root.getID(),'sID')]
        self.get_centres(tree_from, chosen_tree_centres, deps, self.root.getID())
        #print tree_from, chosen_tree_centres, deps
        return map(lambda x: (chosen_tree_centres[x[0]], chosen_tree_centres[x[1]]), deps)


    ################################ TREE DISAMBIGUATION TEMPLATE

    def disamb_tree_rec(self, select_best, node):
        if not node.isTerminal():
            pass
    def disamb_tree(self, select_best):
        pass

    ################################ TREE DISAMBIGUATION FUNCTIONS

    ##################### ACTUAL PCFG

    def getChildrenForActPcfg(self, prod_dict):
        ch = [(prod_dict['centre'], u'true')]
        ch += map(lambda x: (x,u'false'), prod_dict['productions'])
        return str(sorted(map(lambda x:(self.nodes[x[0]].getCategory(), x[1]), ch)))

    def act_pcfg_rec_count_probs(self, grammar, nodes_probs, node_id):
        #print "---------------------", nodes_probs
        if node_id in nodes_probs:
            return nodes_probs[node_id]
        else:
            node = self.nodes[node_id]
            poss_prods = node.getAllChildren()
            if not poss_prods:
                nodes_probs[node_id] = 1.0
                return nodes_probs[node_id]
            for prod_dict in poss_prods:
                prod_dict['prob'] = 1.0
                for ii in prod_dict['productions']:
                    prod_dict['prob'] *= self.act_pcfg_rec_count_probs(grammar, nodes_probs, ii)
                if 'centre' in prod_dict.keys():
                    prod_dict['prob'] *= self.act_pcfg_rec_count_probs(grammar, nodes_probs, prod_dict['centre'])
                try:
                    prod_dict['prob'] *= grammar[(node.getCategory(), str(map(lambda x:(self.nodes[x[0]].getCategory(), x[1]), prod_dict['whole_prod'])))]#self.getChildrenForActPcfg(prod_dict))]
                except KeyError:
                    prod_dict['prob'] *= min(map(lambda x: grammar[x], grammar.keys()))
                    self.missed_prods.append((node.getCategory(), str(map(lambda x:(self.nodes[x[0]].getCategory(), x[1]), prod_dict['whole_prod']))))
            nodes_probs[node_id] = sorted(poss_prods, key=lambda x: -x['prob'])[0]['prob']
            return nodes_probs[node_id]

    def act_pcfg_rec_get_tree(self, node_id):
        node = self.nodes[node_id]
        if node.isTerminal():
            self.disamb_nodes.append(node_id)
        else:
            poss_prods = node.getAllChildren()
            try:
                prod_dict = sorted(poss_prods, key=lambda x: -x['prob'])[0]
            except KeyError:
                prod_dict = poss_prods[0]
            self.disamb_nodes.append( node_id )
            if 'centre' in prod_dict.keys():
                self.act_pcfg_rec_get_tree(prod_dict['centre'])
            for prod in prod_dict['productions']:
                self.act_pcfg_rec_get_tree(prod)

    def act_pcfg(self, grammar):
        nodes_probs = {}
        self.disamb_nodes = []
        self.disamb_dep_nodes = {}
        self.act_pcfg_rec_count_probs(grammar, nodes_probs, self.root.getID())
        self.act_pcfg_rec_get_tree(self.root.getID())
        #self.chosen_dep_nodes = self.get_dep_tree(self.chosen_nodes)
        #self.disamb_dep_nodes = self.get_dep_tree(self.disamb_nodes)
        return self.getStats()#self.precision(), self.recall(), self.precisionStrict(), self.recallStrict(), self.precisionDep()

    def getMissedProds(self):
        res = {}
        for ii in set(self.missed_prods):
            res[ii] = self.missed_prods.count(ii)
        return res


    ################################ EVALUATION
    def no_of_nonterminals(self, tree):
        count = 0
        for ii in tree:
            if not self.nodes[ii].isTerminal():
                count += 1
        return count

    def getStats(self):
        agreed = 0
        for node1 in self.chosen_nodes:
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals_from_to_cat(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
        self.agreed_count = agreed
        agreed = 0
        for node1 in self.chosen_nodes:
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
        self.agreed_strict_count = agreed
        self.chosen_nodes_count = self.no_of_nonterminals(self.chosen_nodes)
        self.disamb_nodes_count = self.no_of_nonterminals(self.disamb_nodes)
        agreed = 0
        for node in self.chosen_dep_nodes:
            if node in self.disamb_dep_nodes:
                agreed += 1
        self.dep_agreed_count = agreed
        self.dep_count = len(self.chosen_dep_nodes)
        return self.agreed_count, self.agreed_strict_count, self.chosen_nodes_count, self.disamb_nodes_count, self.dep_agreed_count, self.dep_count

    def precision(self):
        agreed = 0
        #print sorted(self.chosen_nodes)
        #print sorted(self.disamb_nodes)
        for node1 in self.chosen_nodes:
            #print "*", node1
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals_from_to_cat(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
                    #break
                    #print "#", node2
        self.agreed_count = agreed
        self.chosen_nodes_count = self.no_of_nonterminals(self.chosen_nodes)
        return 1.0 * agreed / self.no_of_nonterminals(self.chosen_nodes)

    def recall(self):
        agreed = 0
        for node1 in self.chosen_nodes:
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals_from_to_cat(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
                    #break
        return 1.0 * agreed / self.no_of_nonterminals(self.disamb_nodes)

    def precisionStrict(self):
        agreed = 0
        for node1 in self.chosen_nodes:
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
                    break
        return 1.0 * agreed / self.no_of_nonterminals(self.chosen_nodes)

    def recallStrict(self):
        agreed = 0
        for node1 in self.chosen_nodes:
            for node2 in self.disamb_nodes:
                if self.nodes[node1].equals(self.nodes[node2]) and not self.nodes[node1].isTerminal():
                    agreed += 1
                    break
        return 1.0 * agreed / self.no_of_nonterminals(self.disamb_nodes)

    def precisionDep(self):
        agreed = 0
        for node in self.chosen_dep_nodes:
            if node in self.disamb_dep_nodes:
                agreed += 1
        return 1.0 * agreed / len(self.chosen_dep_nodes)

    def getProperChildren(self, node_id, tree):
        node = self.nodes[node_id]
        for child in node.getAllChildren():
            if self.childIsChosen(child, tree):
                return child
        return {}

    def equals_from_to_cat(self, node1, node2):
        return self.nodes[node1].equals_from_to_cat(self.nodes[node2])

    def findMistakes_rec(self, node_id):
        for ii in self.disamb_nodes:
            if self.equals_from_to_cat(node_id, ii):
                child1 = self.getProperChildren(node_id, self.chosen_nodes)
                child2 = self.getProperChildren(ii, self.disamb_nodes)
                if self.equals_from_to_cat(child1['centre'], child2['centre']):
                    if not self.nodes[child1['centre']].isTerminal():
                        self.findMistakes_rec(child1['centre'])
                else:
                    self.mistakes.append(('centre', self.nodes[child1['centre']].getCategory(), self.nodes[child2['centre']].getExtCategory()))
                for jj in child1['productions']:
                    for kk in child2['productions']:
                        if self.equals_from_to_cat(jj,kk):
                            if not self.nodes[jj].isTerminal():
                                self.findMistakes_rec(jj)
                        else:
                            if self.nodes[jj].overlaps(self.nodes[kk]):
                                self.mistakes.append(('noncentre', self.nodes[jj].getCategory(), self.nodes[kk].getExtCategory()))

    def findMistakes(self):
        self.mistakes = []
        self.findMistakes_rec(self.root.getID())
        return self.mistakes

    def findMistakesSimpleRec(self, node_id):
        children = self.getProperChildren(node_id, self.disamb_nodes)
        for ii in self.disamb_nodes:
            if self.equals_from_to_cat(node_id, ii):
                child1 = self.getProperChildren(node_id, self.chosen_nodes)
                child2 = self.getProperChildren(ii, self.disamb_nodes)
                if not self.equals_from_to_cat(child1['centre'], child2['centre']):
                    self.mistakes.append(('centre', self.nodes[child1['centre']].getCategory(), self.nodes[child2['centre']].getExtCategory()))
                if not self.nodes[child1['centre']].isTerminal():
                    self.findMistakes_rec(child1['centre'])
                for jj in child1['productions']:
                    for kk in child2['productions']:
                        if self.equals_from_to_cat(jj,kk):
                            if not self.nodes[jj].isTerminal():
                                self.findMistakes_rec(jj)
                        else:
                            if self.nodes[jj].overlaps(self.nodes[kk]):
                                self.mistakes.append(('noncentre', self.nodes[jj].getCategory(), self.nodes[kk].getExtCategory()))


    def findMistakesSimple(self):
        self.mistakes = []
        #self.findMistakesSimple_rec(self.root.getID())
        for node_id in self.disamb_nodes:
            node = self.nodes[node_id]
            children = self.getProperChildren(node_id, self.disamb_nodes)
            #print children
            if children:
                for child in children['productions']:
                    has = False
                    for ii in self.chosen_nodes:
                        if self.equals_from_to_cat(child, ii):
                            has = True
                    if not has:
                        self.mistakes.append(('noncentre', node.getCategory(), self.nodes[child].getCategory()))
                has = False
                for ii in self.chosen_nodes:
                    if self.equals_from_to_cat(children['centre'], ii):
                        has = True
                if not has:
                    self.mistakes.append(('centre', node.getCategory(), self.nodes[children['centre']].getCategory()))
        #print self.mistakes
        return self.mistakes

    def countChildrenZadnie(self):
        res = []
        for node_id in self.chosen_nodes:
            for ii in self.disamb_nodes:
                if self.nodes[node_id].getCategory() == u"zdanie" and self.equals_from_to_cat(node_id, ii):
                    child1 = len(self.getProperChildren(node_id, self.chosen_nodes)['productions'])+int('centre' in self.getProperChildren(node_id, self.chosen_nodes))
                    child2 = len(self.getProperChildren(ii, self.disamb_nodes)['productions'])+int('centre' in self.getProperChildren(ii, self.disamb_nodes))
                    #print self.getProperChildren(node_id, self.chosen_nodes)['whole_prod'], self.getProperChildren(node_id, self.disamb_nodes)['whole_prod']
                    res.append((child1, child2))
        return res

    def getDisambNodes(self):
        return self.disamb_nodes
