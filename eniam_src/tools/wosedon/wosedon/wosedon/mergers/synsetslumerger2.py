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

from collections import defaultdict

from wosedon.basegraph import BaseGraph
from mergerinterface import MergerInterface

class SynsetsLUMerger2(MergerInterface):
  """!
  Class for adding edges from lexical units graph to synsets graph. 

  The purpose of this class is to add edges from lexical units graph to synsets 
  graph. New edges between synsets are added according to relation between lexical 
  units edges. Thus, each edge from lexical units graph is inserted to the synsets graph.
  """
  def __init__(self, resources, options, str_name = 'SynsetsLUMerger2'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsLUMerger2"
    """
    super(SynsetsLUMerger2, self).__init__(resources, options, str_name)

  def merge(self, g1, g2):
    """!
    Only edges from lexical units graph are added to synsets graph. The final 
    graph contain one type of nodes, namely synsets nodes. Each synset node has 
    an attribute named "synset", to which is assigned synset object. Synset object 
    consist of: synset_id and lu_set, which contains lexical units objects. Whereas 
    lexical units objects consists of: lu_id, lemma, pos, variant and domain. The final 
    graph contain two types of relation. First type is relation occurring between synsets 
    nodes only, which have assigned relation identifier from PLWN. Second type is 
    relation occurring between lexical units, but proper synsets nodes are connected. 
    If two lexical units are connected in lexical units graph and they exist in 
    lu_set in some synsets then synsets nodes will be connected. This connection has 
    two attributes: "rel_id" and "rel_type". "rel_id" is a relation identifier 
    from lexical units graph, whereas to "rel_type" is assigned word "lu".

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    @param g1 - synsets graph
    @param g2 - lexical units graph
    @return object of BaseGraph class
    """
    g = BaseGraph()
    g.copy_graph_from(g1)
    
    lu_on_vertex_dict = defaultdict(set)
    for node in g.all_nodes():
      lu_set = node.synset.lu_set
      for lu in lu_set:
        lu_on_vertex_dict[lu.lu_id].add(node)
    
    for edge in g2.all_edges():
      parent_lu_id = edge.source().lu.lu_id
      parent_node_set = None
      if lu_on_vertex_dict.has_key(parent_lu_id):
        parent_node_set = lu_on_vertex_dict[parent_lu_id]
      else:
        continue

      child_lu_id = edge.target().lu.lu_id
      child_node_set = None
      if lu_on_vertex_dict.has_key(child_lu_id):
        child_node_set = lu_on_vertex_dict[child_lu_id]
      else:
        continue
      
      for p_node in parent_node_set:
        for ch_node in child_node_set:
          g.add_edge(p_node, 
                     ch_node, 
                     [("rel", edge.rel)], 
                     simply = True)

    return g
