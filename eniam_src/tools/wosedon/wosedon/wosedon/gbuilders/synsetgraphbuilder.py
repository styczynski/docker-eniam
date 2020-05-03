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

import os
import operator
from wosedon.basegraph import BaseGraph
from graphbuilderinterface import GraphBuilderInterface

class SynsetGraphBuilder(GraphBuilderInterface):
  """!
  Class for building synset graph.

  The built graph is based on synset graph file. The synset graph is unpickled 
  from file and according to build options set in config file some nodes and edges 
  are filtered out.
  """
  def __init__(self, resources, options, str_name = 'SynsetGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "SynsetGraphBuilder"
    """
    super(SynsetGraphBuilder, self).__init__(resources, options, str_name)

  def build_graph(self):
    """!
    Build synset graph. 

    At the beginning the synset graph is unpickled from file, which path is set in 
    config file. Next some nodes and edges are filtered out according to build options, 
    which is also set in config file. Each node has an attribute named "synset" which 
    contain synset object. Synset object consist of the following fields: synset_id and 
    lu_set, which contains lexical units objects. Whereas to each edge is assigned relation 
    identifier acquired from PLWN, which occurs between linked synsets.

    The directness of the graph and uniqueness of edges is set in the config file. 
    
    Warning: Nodes (synsets) is filtered out according to part of speech (POS). Each synset contain 
    a set of lexical units. Whereas each lexical unit have assigned one POS. Thus, before node is 
    filtered out the most frequent POS in its lexical units set is checked.

    @return object of BaseGraph class
    """
    syn_graph_file_path = \
      self.resources().plwn_graph_file() + '_syn.xml.gz'

    if not os.path.exists(syn_graph_file_path):
      raise IOError(
        "%s file not found!" % \
          syn_graph_file_path)

    g = BaseGraph()
    g.init_graph(drctd = self.options().is_directed_graph())
    g.unpickle(syn_graph_file_path)
    g.set_directed(self.options().is_directed_graph())

    # Vertices filter
    if self.options().accept_pos() or self.options().accept_lexicon():
      nodes_to_filter_set = set()
      for n in g.all_nodes():
        lu_set = n.synset.lu_set

        if self.options().accept_lexicon():
          lexicon = None if not lu_set else (list(lu_set)[0].lexicon \
            if len(lu_set) == 1 else self._get_most_frequent_lexicon(lu_set))
          if not lexicon in self.options().accept_lexicon():
            nodes_to_filter_set.add(n)

        if self.options().accept_pos():
          pos = None if not lu_set else (list(lu_set)[0].pos \
            if len(lu_set) == 1 else self._get_most_frequent_pos(lu_set))
          if not pos in self.options().accept_pos():
            nodes_to_filter_set.add(n)

      g.nodes_filter(nodes_to_filter_set)

    # Edges
    # Add reversed edges
    
    lst = set()
    for edge in g.all_edges():
      if edge.rel_id in self.options().get_add_reversed_edges():
        lst.add((edge.target(), edge.source(), edge.rel_id))
    for s, t, i in lst:
      ne = g.add_edge(s, t, simply=True)
      ne.rel_id = self.options().get_add_reversed_edges()[i]
    
    # Check edges duplicates
    if self.options().has_unique_edges():
      g.remove_edge_duplicates()
      
    # Edges filter
    if self.options().syn_rel_ids():
      edges_to_filter_set = set()
      for e in g.all_edges():
        if not e.rel_id in self.options().syn_rel_ids():
          edges_to_filter_set.add(e)
      g.edges_filter(edges_to_filter_set)
    
    
    # Type of relation
    g.create_edge_attribute("rel", "string")
    for e in g.all_edges():
      e.rel = 's' + str(e.rel_id)

    return g

  def _get_most_frequent_pos(self, lu_set):
    """!
    Function return the most frequent part of speech in the given lexical units set.

    @param lu_set - set of lexical units objects
    @return the most frequent part of speech in lexical units set
    """
    pos_dict = {}
    for lu in lu_set:
      if lu.pos not in pos_dict:
        pos_dict[lu.pos] = 0
      pos_dict[lu.pos] += 1
    return max(pos_dict.iteritems(), key=operator.itemgetter(1))[0]

  def _get_most_frequent_lexicon(self, lu_set):
    """!
    Function return the most frequent lexicon in the given lexical units set.

    @param lu_set - set of lexical units objects
    @return the most frequent lexicon in lexical units set
    """
    lexicon_dict = {}
    for lu in lu_set:
      if lu.lexicon not in lexicon_dict:
        lexicon_dict[lu.lexicon] = 0
      lexicon_dict[lu.lexicon] += 1
    return max(lexicon_dict.iteritems(), key=operator.itemgetter(1))[0]
