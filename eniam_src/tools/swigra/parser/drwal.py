#! /usr/bin/env python


'''

This script recognises the same kinds of trees from quite far away and then
cuts all of them down until only one identical tree remains. Therefore,
it's a lumberjack script (and it's OK). 

@author: Piotr Sikora
'''

#TODO: read arguments, decide if they're ok, run the script on them
#arguments to handle: debug, path to file to be processed

import sys, getopt

import time
from lxml import etree
from copy import deepcopy

debug = False
timers = {}

def main(argv):
    try:           
        opts, args = getopt.getopt(argv, "d", ["debug"])
    except getopt.GetoptError:          
        usage()                         
        sys.exit(2)
    if not args:
        usage()
        sys.exit(2)                     
    for opt, _ in opts:                
        if opt in ("-d", "--debug"):
            debug = True      
    target_file = "".join(args)
    process_file(target_file)


def usage():
    print "Improper arguments"

def process_file(file):
    global number_of_unifiable_sets
    global same_rule_symbol
    global different_rule_symbol
    global number_of_same_realizations
    number_of_unifiable_sets = 0
    same_rule_symbol = {}
    different_rule_symbol = {}
    number_of_same_realizations = {}
    find_same_trees(file)
    print_stats()

''' 
Utilities
'''

def keep_time(string, start_time):
    timer = time.time() - start_time
    if string in timers:
        timers[string] = timers[string] + timer
    else:
        timers[string] = timer
    
def dictionarize_features(list_of_features):
    features = {}
    for f in list_of_features:
        features[f.attrib['type']] = f.text
    return features

def is_terminal(node):
    if node.getchildren()[0].tag == 'terminal':
        return True
    else:
        return False 

def nid(child):
    return child.attrib['nid']

def compact_string_realization(name, realization):
    string = name + ": " + realization.attrib['rule'] + "  "
    for child in realization.getchildren():
        string += child.attrib['nid'] + '  '
    return string

def compact_string_node(node):
    string = "Node: " + str(node.attrib) + '\n'
    string += etree.tostring(node.getchildren()[0]) + '\n'
    for realization in node.getchildren()[1:]:
        string += compact_string_realization("<children>",realization) + '\n'
    return string

    
def gather_info(node, same_realizations):
    #NUMBER OF SAME REALIZATIONS
    if len(same_realizations) in number_of_same_realizations:
        number_of_same_realizations[len(same_realizations)] = number_of_same_realizations[len(same_realizations)] + 1
    else: 
        number_of_same_realizations[len(same_realizations)] = 1
    #RULES
    same_realizations_copy = same_realizations.copy() 
    rule = same_realizations_copy.pop().attrib['rule']
    same_rule = True
    rules = [rule]
    for realization in same_realizations_copy:
        if realization.attrib['rule']!=rule:
            same_rule = False
        rules.append(realization.attrib['rule'])
    if same_rule:
        if rule in same_rule_symbol:
            same_rule_symbol[rule] = same_rule_symbol[rule] + 1
        else:
            same_rule_symbol[rule] = 1
    else:
        if tuple(sorted(rules)) in different_rule_symbol:
            different_rule_symbol[tuple(sorted(rules))] = different_rule_symbol[tuple(sorted(rules))] + 1
        else:
            different_rule_symbol[tuple(sorted(rules))] = 1  

def alarm(string):
    sys.exit(string)
    

def print_stats():
    print "Number of same realizations: " + str(number_of_same_realizations)
    print "Different rule symbol: " + str(different_rule_symbol) 
    print "Same rule symbol: " + str(same_rule_symbol)
    print "Number of unifiable sets: " + str(number_of_unifiable_sets)
    if debug:
        for key in timers:
            print "Timer " + key + " : " + str(timers[key])


''' 
Program logic
'''

'''

Recounting subtrees module

'''

def count_subtrees(node, nodes, subtrees_of_nodes):
    subtrees = 0
    for realization in node.iter('children'):
        subtrees_realization = 1
        for child in realization.iter('child'):
            cnid = nid(child)
            if cnid not in subtrees_of_nodes:
                count_subtrees(nodes[cnid], nodes, subtrees_of_nodes)
            subtrees_realization *= subtrees_of_nodes[cnid]
        subtrees += subtrees_realization
    node.attrib['subtrees'] = str(subtrees)
    subtrees_of_nodes[nid(node)] = subtrees

def recount_subtrees(tree):
    nodes = {}
    subtrees_of_nodes = {}
    startnode_attrib = tree.getroot().find('startnode').attrib
    startfrom = startnode_attrib['from'] 
    startto = startnode_attrib['to']
    startnode = None
    for node in tree.getroot().iter(tag='node'):
        if node.attrib['from'] == startfrom and node.attrib['to'] == startto:
            startnode = node
        nodes[nid(node)] = node
        if is_terminal(node):
            subtrees_of_nodes[nid(node)] = 1
    count_subtrees(startnode, nodes, subtrees_of_nodes)


def renumerate_nodes(tree, holes):
    forest = tree.getroot()
    nodes = {}
    nodecount = 0
    for node in forest.iter(tag = 'node'):
        nodes[int(nid(node))] = node
        nodecount +=1
    stats = tree.getroot().find('stats')
    stats.attrib['nodes'] = str(nodecount)
    sorted_nids = sorted(nodes.keys(), reverse = True)
    holes = sorted(holes)
    nodes_to_renumerate = {}
    filled_holes = 0
    while holes:
        node_id = sorted_nids[filled_holes]
        hole_id = holes.pop(0)  
        if hole_id > node_id:
            break
        nodes_to_renumerate[node_id] = hole_id
        filled_holes += 1
    for node_id in nodes_to_renumerate:
        nodes[node_id].attrib['nid'] = str(nodes_to_renumerate[node_id])
    for child in forest.iter(tag='child'):
        child_id = int(nid(child))
        if child_id in nodes_to_renumerate:
            child.attrib['nid'] = str(nodes_to_renumerate[child_id])
''' 
Program logic
'''

def are_same_nodes(node1,node2):
    noded1 = node1.getchildren()[0] #(non)terminal
    noded2 = node2.getchildren()[0] # -"-
    if noded1.tag != noded2.tag:
        return False
    if noded1.tag == 'terminal': # both tags the same, check one
        orth1, base1, feat1 = noded1.getchildren()
        orth2, base2, feat2 = noded2.getchildren()
        if orth1.text!=orth2.text:
            return False
        if base1.text!=base2.text:
            return False
        if feat1.attrib != feat2.attrib or feat1.text!=feat2.text:
            return False
        return True
    if noded1.tag == 'nonterminal':
        if len(noded1.getchildren())!=len(noded2.getchildren()):
            return False
        if noded1.getchildren()[0].text != noded2.getchildren()[0].text:
            return False
        features1 = dictionarize_features(noded1.getchildren()[1:]) 
        features2 = dictionarize_features(noded2.getchildren()[1:])
        if sorted(features1.keys())!=sorted(features2.keys()):
            return False
        if sorted(features1.values())!=sorted(features2.values()):
            return False
    return True

def are_potential_same_realizations(realization1,realization2,nodes):
    ''' realizations consist of <children> elements of the forest'''
    #same_nodes = set([True])
    if debug:
        timer = time.time()
    if len(realization1.getchildren())!=len(realization2.getchildren()):
        return False
    for child1, child2 in zip(realization1.getchildren(),realization2.getchildren()):
        if child1.attrib['from']!=child2.attrib['from'] or child1.attrib['to']!=child2.attrib['to'] or child1.attrib['head']!=child2.attrib['head']:
            return False
    if debug:
        keep_time('\t\t iterating through <Children>',timer)
    raise_nonterminal_alarm = False
    for child1, child2 in zip(realization1.getchildren(),realization2.getchildren()):
        if child1.attrib['nid']!=child2.attrib['nid']:
            if debug:
                timer = time.time()
            if not are_same_nodes(nodes[child1.attrib['nid']],nodes[child2.attrib['nid']]):
                if debug:
                    keep_time('\t\t are same nodes',timer)
                return False
            else:
                if debug:
                    keep_time('\t\t are same nodes',timer)
                if realization1.attrib['rule']!=realization2.attrib['rule']:
                    if debug:
                        terminal_timer = time.time()
                    if not is_terminal(nodes[child1.attrib['nid']]) or not is_terminal(nodes[child2.attrib['nid']]):
                        raise_nonterminal_alarm = True
                    if debug:
                        keep_time('\t\t checking terminality of same nodes', terminal_timer)
                #same_nodes.add(tuple(sorted([child1.attrib['nid'],child2.attrib['nid']])))
    if raise_nonterminal_alarm:
        alarm("Identical nonterminals in differently ruled realizations: " + etree.tostring(realization1) + " " + etree.tostring(realization2))
    return True #in case there's two realizations with exact same nodes including nids

def compute_same_node_sets(same_realizations, nodes):
    same_realizations_copy = same_realizations.copy()
    first_realization = same_realizations_copy.pop()
    length_of_realization = len(first_realization.getchildren())
    same_node_sets = set()
    for i in range(length_of_realization):
        nid = first_realization.getchildren()[i].attrib['nid']
        same_node_set = set()
        for realization in same_realizations_copy:
            nid2 = realization.getchildren()[i].attrib['nid']
            if nid!=nid2:
                same_node_set.add(nid)
                same_node_set.add(nid2)
        if same_node_set:
            same_node_tuple = tuple(sorted(same_node_set))
            same_node_sets.add(same_node_tuple)
    return same_node_sets

def check_node_for_same_realizations(node, nodes):
    if debug:
        timer = time.time()
    realizations = []
    list_of_same_realizations = []
    for subnode in node.iterchildren():
        if subnode.tag == 'children':
            realizations.append(subnode)
    if debug:
        keep_time('\t node.iterchildren()', timer)
    if len(realizations) > 1:
        if debug:
            timer = time.time()
        for realization1 in realizations:
            same_realizations = set()
            for realization2 in realizations[realizations.index(realization1) + 1:]:
                if realization1 != realization2:
                    if debug:
                        keep_time('\t iterating through realizations',timer)
                    if are_potential_same_realizations(realization1, realization2, nodes):
                        same_realizations.add(realization1)
                        same_realizations.add(realization2)
                    if debug:
                        timer = time.time()
            if same_realizations:
                flag = True
                for s_r in list_of_same_realizations:
                    if same_realizations <= s_r:
                        flag = False
                if flag:
                    list_of_same_realizations.append(same_realizations)
    same_nodes = {} 
    ''' a list of node sets that might be unified '''
    for same_realizations in list_of_same_realizations:
        if debug:
            timer = time.time()
        new_same_nodes = compute_same_node_sets(same_realizations, nodes)
        for same_node_tuple in new_same_nodes:
            if same_node_tuple in same_nodes:
                same_nodes[same_node_tuple].append((nid(node),same_realizations))
            else:
                same_nodes[same_node_tuple] = [(nid(node),same_realizations)]
        if debug:
            keep_time('\t compute_same_node_sets',timer)
            timer = time.time()
        gather_info(node, same_realizations)
        if debug:
            keep_time('\t gather_info', timer)
    if debug:
        timer = time.time()
    """
    for same_node_set in same_nodes:
        write_same_nodes(same_node_set,nodes)
    if debug:
        keep_time('\t writing same_node_sets',timer)
    """
    return list_of_same_realizations, same_nodes

def is_unifiable(same_node_tuple, same_nodes, node_refs, same_realizations_by_nid):
    '''
    
    Decides whether nodes identified in a same_node_tuple can be unified or whether they need
    to be replaced.
    
    Same_nodes is a dictionary of same node sets pointing to lists of tuples, in which
    the first element is a node id and the second is a set of realizations for that node
    in which a given set of same nodes is identical.
    
    node_refs is a dictionary of all realizations referencing a given node 
        
    s_r_by_nid has a list of sets of same realizations in a given node
    
    The solution:
    If the set of realizations referencing nodes from a given set of identical nodes is the same
    as the set of realizations in which the given set of nodes was found to be identical, unify 
    these nodes. Otherwise replace them with a new node.
    
    This works perfectly for the following cases:
    Identical nodes always appear all together in identical sets of realizations.
    Any one of the nodes appears in realizations, which are not identical to realizations 
    containing other nodes from the set.
    
    However, this solution is not perfect in the following cases:
    1) Consider the case, where nodes n1 & n2 appear in two sets of identical nodes:
    (n1, n2, n3)
    (n1, n2)
    The sets of references to n1 & n2 will be different for those two sets of identical nodes, therefore
    both sets will be replaced with a new node and all the nodes here will be deleted. 
    
    2) There are 3+ nodes in the set and some of them always appear together in identical realizations,
    while there is at least one that does appear in an unrelated realization. In this case, all nodes will
    be replaced with a new node and then - if non-referenced nodes are to be cleaned up - nodes always
    appearing together will be deleted, while they could be just folded into one of them.
    
    All in all, this is a small problem, assuming clean-up of non-referenced nodes after processing the forest,
    as it only causes not-strictly-necessary deletion of nodes. It can be solved with a renumeration of nodes
    after the processing. 
    
    '''
    same_realizations = set()
    for node_id, set_of_realizations in same_nodes[same_node_tuple]:
        for realization in set_of_realizations:
            same_realizations.add((node_id, realization))
    referencing_realizations = set()
    for same_node_id in same_node_tuple:
        referencing_realizations |= set(node_refs[same_node_id])
    if same_realizations == referencing_realizations:
        return True
    return False
    
    
same_nodes_number = []
nodes_number = []

def are_realizations_identical(realizations):
    set_of_realizations = set(realizations).copy()
    head_realization = set_of_realizations.pop()
    for realization in set_of_realizations:
        for child1, child2 in zip(realization.getchildren(),head_realization.getchildren()):
            if child1.attrib != child2.attrib:
    #            output.write(str(child1.attrib['nid']) + ' , ' + str(child2.attrib['nid']) + '\n')
                return False
    return True
   
def modify_realization(old_id, new_id, realization):
    success = False
    for child in realization:
        if nid(child) == old_id:
            child.attrib['nid'] = new_id
            success = True
    return success

def extract_nids_of_children(realization):
    children = set()
    for child in realization.iter(tag='child'):
        children.add(nid(child))
    return children

def process_nodes_in_forest(tree, nodes_to_process, nodes, max_nid):
    forest = tree.getroot()
    same_nodes = {}
    same_realizations_by_nid = {}
    new_nodes_to_process = []
    holes = []
    for node in nodes_to_process:
        new_same_realizations, new_same_nodes = check_node_for_same_realizations(node, nodes)
        if new_same_realizations:
            same_realizations_by_nid[node.attrib['nid']] = new_same_realizations
        for same_node_tuple in new_same_nodes:
            if same_node_tuple in same_nodes:
                same_nodes[same_node_tuple].extend(new_same_nodes[same_node_tuple])
            else:
                same_nodes[same_node_tuple] = new_same_nodes[same_node_tuple]
#diagnostics & checking which same_node_sets are ok for unification
    if debug:
        timer = time.time()

    same_node_refs = {}
    for same_node_set in same_nodes:
        for same_node in same_node_set:
            same_node_refs[same_node] = []
    same_nodes_number.append(len(same_node_refs.keys()))
    if debug:
        keep_time('processing same_node_sets', timer)
        timer = time.time()
    for node in forest.iter('node'):
        for realization in node.iter('children'):
            for child in realization.iter('child'):
                cnid = nid(child)
                if cnid in same_node_refs:
                    same_node_refs[cnid].append((nid(node), realization))
    if debug:
        keep_time('gathering references to same nodes', timer)
        timer = time.time()
    
    """
    Algorithm of unifying realisations:
    
    For each set of identical nodes:
    1) Choose head_node for a set of identical nodes.
    2) Replace references to other nodes in same realizations with references to head node.
    Update the dictionary of references to those other nodes.
    
    After all such operations sets of same realizations should consist of truly identical
    realizations, with all the exact same nids. Then, they need to be all removed until just
    one for each set remains.
    """
    nodes_to_unify = []
    nodes_to_replace = []
    for same_node_tuple in same_nodes:
        same_node_set = set(same_node_tuple)
        head_node_id = same_node_set.pop()
        if is_unifiable(same_node_tuple, same_nodes, same_node_refs, same_realizations_by_nid):
            nodes_to_unify.append((head_node_id, same_node_set))
            for tail_node_id in same_node_set:
                for node_id, realization in list(same_node_refs[tail_node_id]):
                    modify_realization(tail_node_id, head_node_id,realization)
        else:
            nodes_to_replace.append((head_node_id, same_node_set, same_node_tuple))
            for tail_node_id in same_node_set:
                #only pruning realizations from appropriate sets of same realizations
                for node_id, set_of_same_realizations in same_nodes[same_node_tuple]:
                    for realization in set_of_same_realizations:
                        modify_realization(tail_node_id, head_node_id, realization)
    if debug:
        keep_time('making same realizations identical', timer)
        timer = time.time()
    for node_id in same_realizations_by_nid:
        node = nodes[node_id]
        for realizations in same_realizations_by_nid[node_id]:
            if not are_realizations_identical(realizations):
                alarm("Non-identical realizations!")
            save = realizations.pop()
            for realization in realizations:
                node.remove(realization)
            realizations.add(save)
    if debug:
        keep_time('removing superfluous realizations', timer)
        timer = time.time()
    for head_node_id, node_set in nodes_to_unify:
        head_node = nodes[head_node_id]
        for tail_node_id in node_set:
            for children in nodes[tail_node_id].iter('children'):
                head_node.append(children)
            if nodes[tail_node_id] in forest:
                forest.remove(nodes[tail_node_id])
                holes.append(int(tail_node_id))
                del same_node_refs[tail_node_id]
            else:
                alarm("Node not in forest: " + str(tail_node_id))
        new_nodes_to_process.append(head_node)
    if debug:
        keep_time('unifying nodes', timer)
        timer = time.time()
    """ 
    
    If a node that will be replaced has a node that will be replaced too
    among its children, the deeper node has to be replaced first.

    Therefore, sets of same nodes which have no (head) nodes to replace among
    the children that are to be unified/replaced should be replaced first.
     
    """    
    children_of_nodes_to_replace = {}
    head_ids = []
    for head_node_id, _, same_node_tuple in nodes_to_replace:
        head_ids.append(head_node_id)
        children_of_nodes_to_replace[same_node_tuple] = set()
        for node_id in same_node_tuple:
            if node_id in same_realizations_by_nid:
                for realizations in same_realizations_by_nid[node_id]:
                    for realization in realizations:
                        children_of_nodes_to_replace[same_node_tuple] |= extract_nids_of_children(realization)
    
    while nodes_to_replace:
        free_nodes_to_replace = []
        for ntr_tuple in nodes_to_replace:
            _, _, same_node_tuple = ntr_tuple
            if children_of_nodes_to_replace[same_node_tuple].isdisjoint(set(head_ids)):
                free_nodes_to_replace.append(ntr_tuple)
        
        for head_node_id, same_node_set, same_node_tuple in free_nodes_to_replace:
            nodes_to_replace.remove((head_node_id, same_node_set, same_node_tuple))
            head_ids.remove(head_node_id)

            head_node = nodes[head_node_id]
            new_node = deepcopy(head_node)
            new_node.attrib['nid'] = str(max_nid + 1)
            max_nid += 1
            for node_id, set_of_same_realizations in same_nodes[same_node_tuple]:
                for realization in set_of_same_realizations:
                    modify_realization(head_node_id,nid(new_node),realization)
            for other_node_id in same_node_set:
                for children in nodes[other_node_id].iter('children'):
                    new_node.append(deepcopy(children))
            new_nodes_to_process.append(new_node)
            nodes[nid(new_node)] = new_node
            forest.append(new_node)
    if debug:
        keep_time('creating replacement nodes', timer)
        timer = time.time()
    node_refs = {}
    for id in same_node_refs:
        node_refs[id] = False
    for child in forest.iter('child'):
        cnid = nid(child)
        if cnid in node_refs:
            node_refs[cnid] = True
    for id in node_refs:
        if not node_refs[id]:
            forest.remove(nodes[id])
            holes.append(int(id))
    if debug:
        keep_time('deleting unreferenced nodes', timer)
    return (max_nid, new_nodes_to_process, holes)

def find_same_trees(file):
    print 'Processing file: ' + file
    if debug:
        timer = time.time()
    tree = etree.parse(file)
    if debug:
        keep_time('parsing',timer)
        timer = time.time()
    nodes = {}
    max_nid = -1;
    for node in tree.getroot().iter(tag='node'):
        node_nid = node.attrib['nid']
        if int(node_nid) > max_nid:
            max_nid = int(node_nid)
        nodes[node_nid] = node
    if debug:
        keep_time('constructing a dictionary of nodes', timer)
        
    nodes_to_process = nodes.values()
    if nodes_to_process:
        holes = []
        while nodes_to_process:
            nodes_number.append(len(nodes.values()))
            max_nid, nodes_to_process, new_holes = process_nodes_in_forest(tree, nodes_to_process, nodes, max_nid)
            holes.extend(new_holes)
        recount_subtrees(tree)
        renumerate_nodes(tree, holes)
        if debug:
            timer = time.time()
        if debug:
            keep_time('recounting subtrees', timer)
        outFile = open(file,'w')
        outFile.write(etree.tostring(tree, encoding='UTF-8', pretty_print = True, xml_declaration = True))


if __name__ == "__main__":
    main(sys.argv[1:])