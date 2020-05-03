#
# <one line to give the library's name and an idea of what it does.>
# Copyright (C) 2014  <copyright holder> <email>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
#

import sys
from collections import defaultdict

from wosedon.basegraph import BaseGraph
from mergerinterface import MergerInterface

class SynsetsMSRMerger(MergerInterface):
  """!
  Class for merging synsets graph and MSR graph.

  MSR graph was built based on the k-best file, which is described in MSRGraphBuilder class. 
  Each node represents one word, which consist of word lemma and part of speech. Whereas 
  each edge represents relation described by relatedness weight.

  The purpose of this class is to merge two graphs, namely synsets graph and 
  MSR graph. Nodes from MSR graph are conected to proper nodes from synsets graph, 
  which have them in their lexical units sets.
  """
  def __init__(self, resources, options, str_name = 'SynsetsMSRMerger'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsMSRMerger"
    """
    super(SynsetsMSRMerger, self).__init__(resources, options, str_name)

  def merge(self, g1, g2):
    """!
    Merge two given graphs, namely synsets graph and MSR graph. The final 
    graph contain two types of nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas lexical units objects consists of: lu_id, 
    lemma, pos, variant and domain. Each MSR node has an attribute named "msr", to which is 
    assigned a word (word_lemma/POS). To the relation between synsets nodes is assigned 
    relation identifier from PLWN. Whereas to the relation between MSR nodes is assigned 
    weight, which describe a power of relatedness between linked words. New relations 
    are added between synsets nodes and MSR nodes. If word_lemma/POS of MSR node occur 
    in the object from lu_set of synset node then new relation is added between them.

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    Warning: If the final graph is a directed graph then two edges will be added between 
    a synset node and a MSR node. First edge will be add from synset node to MSR node 
    and second from MSR node to synset node.

    @param g1 - synsets graph
    @param g2 - MSR graph
    @return object of BaseGraph class
    """
    g = BaseGraph()
    g.init_graph(drctd = g1.is_directed())

    g.merge_graphs(g1, g2)

    lemma_on_vertex_dict = defaultdict(set)
    for node in g.all_nodes():
      if node.synset:
        lu_set = node.synset.lu_set
        for lu in lu_set:
          lemma_on_vertex_dict[lu.lemma + '/' + str(lu.pos)].add(node)

    for node in g.all_nodes():
      if node.msr:
        lemma_pos = node.msr
        if lemma_on_vertex_dict.has_key(lemma_pos):
          for synset_id_node in lemma_on_vertex_dict[lemma_pos]:
            g.add_edge(node, synset_id_node, [("rel", 'syn-msr')], simply = True)
            if g.is_directed():
              g.add_edge(synset_id_node, node, [("rel", 'syn-msr')], simply = True)
        else:
          logging.getLogger(__name__).warning("No synset in the graph contains lemma '%s'.", lemma_pos)

    return g
