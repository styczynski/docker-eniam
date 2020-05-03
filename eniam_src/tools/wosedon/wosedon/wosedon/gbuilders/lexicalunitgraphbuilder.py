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
from wosedon.basegraph import BaseGraph
from graphbuilderinterface import GraphBuilderInterface

class LexicalUnitGraphBuilder(GraphBuilderInterface):
  """!
  Class for building lexical unit graph.

  The built graph is based on lexical unit graph file. The lexical unit graph is 
  unpickled from file and according to build options set in config file some nodes 
  and edges are filtered out.
  """
  def __init__(self, resources, options, str_name = 'LexicalUnitGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "LexicalUnitGraphBuilder"
    """
    super(LexicalUnitGraphBuilder, self).__init__(resources, options, str_name)

  def build_graph(self):
    """!
    Build lexical unit graph. 

    At the beginning the lexical unit graph is unpickled 
    from file, which path is set in config file. Next some nodes and edges are 
    filtered out according to build options, which is also set in config file. Each 
    node has an attribute named "lu" which contain lexical unit object. Lexical unit 
    object consist of the following fields: lu_id, lemma, pos, domain and variant. Whereas 
    to each edge is assigned relation identifier acquired from PLWN, which occurs 
    between linked lexical units.

    The directness of the graph and uniqueness of edges is set in the config file. 

    @return object of BaseGraph class
    """
    lu_graph_file_path = \
      self.resources().plwn_graph_file() + '_lu.xml.gz'

    if not os.path.exists(lu_graph_file_path):
      raise IOError(
        "%s file not found!" % \
          lu_graph_file_path)

    g = BaseGraph()
    g.init_graph(drctd = self.options().is_directed_graph())
    g.unpickle(lu_graph_file_path)
    g.set_directed(self.options().is_directed_graph())

    # Vertices filter.
    if self.options().accept_pos() or self.options().accept_lexicon():
      nodes_to_filter_set = set()
      for n in g.all_nodes():
        if self.options().accept_lexicon():
          lexicon = n.lu.lexicon
          if not lexicon in self.options().accept_lexicon():
            nodes_to_filter_set.add(n)

        if self.options().accept_pos():
          pos = n.lu.pos
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
    if self.options().lu_rel_ids():
      edges_to_filter_set = set()
      for e in g.all_edges():
        if not e.rel_id in self.options().lu_rel_ids():
          edges_to_filter_set.add(e)
      g.edges_filter(edges_to_filter_set)
    
    # Type of relation
    g.create_edge_attribute("rel", "string")
    for e in g.all_edges():
      e.rel = 'l' + str(e.rel_id)

    return g
