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

class SynsetsLUMerger(MergerInterface):
  """!
  Class for merging synsets graph and lexical units graph.

  The purpose of this class is to merge two graphs, namely synsets graph and 
  lexical units graph. Nodes from lexical units graph are connected to proper 
  nodes from synsets graph, which have them in their lexical units sets.
  """
  def __init__(self, resources, options, str_name = 'SynsetsLUMerger'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsLUMerger"
    """
    super(SynsetsLUMerger, self).__init__(resources, options, str_name)

  def merge(self, g1, g2):
    """!
    Merge two given graphs, namely synsets graph and lexical units graph. The final 
    graph contain two types of nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas each lexical unit node has an 
    attribute named "lu", to which is assigned lexical unit object. Lexical unit 
    object consist of: lu_id, lemma, pos, variant and domain. Synset relation exist 
    between synsets nodes only. Lexical unit relation exist between lexical units nodes 
    only. Whereas new relations are added between synsets nodes and lexical units 
    nodes. If lu_id of lexical unit node occur in the object from lu_set of synset 
    node then new relation is added between them.
    
    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    Warning: If the final graph is a directed graph then two edges will be added between 
    a synset node and a lexical unit node. First edge will be add from synset node to 
    lexical unit node and second from lexical unit node to synset node.

    @param g1 - synsets graph
    @param g2 - lexical units graph
    @return object of BaseGraph class
    """
    g = BaseGraph()
    g.init_graph(drctd = g1.is_directed())

    g.merge_graphs(g1, g2)

    lu_on_vertex_dict = defaultdict(set)
    for node in g.all_nodes():
      if node.synset:
        lu_set = node.synset.lu_set
        for lu in lu_set:
          lu_on_vertex_dict[lu.lu_id].add(node)

    for node in g.all_nodes():
      if node.lu:
        lu_id = node.lu.lu_id
        if lu_on_vertex_dict.has_key(lu_id):
          for synset_id_node in lu_on_vertex_dict[lu_id]:
            g.add_edge(node, synset_id_node, [("rel", 'syn-lu')], simply = True)
            if g.is_directed():
              g.add_edge(synset_id_node, node, [("rel", 'syn-lu')], simply = True)
        else:
          logging.getLogger(__name__).warning("No synset in the graph contains lemma of ID %d.", lu_id)

    return g
