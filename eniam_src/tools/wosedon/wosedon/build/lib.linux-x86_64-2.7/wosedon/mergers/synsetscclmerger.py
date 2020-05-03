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

import sys, logging

from wosedon.basegraph import BaseGraph
from mergerinterface import MergerInterface

class SynsetsCCLMerger(MergerInterface):
  """!
  Class for merging ccl graph and SUMO graph. Synsets from synset graf are additionaly
  connected with edges from ccl graph.
  """
  def __init__(self, resources, options, str_name = 'SynsetsCCLMerger'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsCCLMerger"
    """
    super(SynsetsCCLMerger, self).__init__(resources, options, str_name)


  def merge(self, g1, g2):
    """!
    @param g1 - synsets graph
    @param g2 - CCL graph
    @return object of BaseGraph class
    """
    g = BaseGraph()
    g.copy_graph_from(g1)
    
    for edge in g2.all_edges():
      source_node = g.get_node_for_synset_id(edge.source().synset_id)
      target_node = g.get_node_for_synset_id(edge.target().synset_id)
      
      if not source_node:
        logging.getLogger(__name__).warning("Ccl graph contains unknown synset: %d", edge.source().synset_id)
        
      if not target_node:
        logging.getLogger(__name__).warning("Ccl graph contains unknown synset: %d", edge.target().synset_id)
      
      if source_node and target_node:
        ne = g.add_edge(source_node, target_node, simply=True)
        ne.rel = edge.rel

    return g



























