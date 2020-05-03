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
from graphbuilderinterface import  GraphBuilderInterface

class WNDGraphBuilder(GraphBuilderInterface):
  """!
  Class for building WND graph. 

  The built graph is based on WordNet Domains (WND) graph file. WordNet Domains is a 
  lexical resource created in a semi-automatic way by augmenting WordNet with domain 
  labels. In addition WND has a hierarchical structure, thus between concepts there 
  are only one relation namely isa.

  Format of WND graph file is presented below. First collumn in this file represent 
  parent name, second collumn represent child name, while last column represent 
  relation type between them.

  Format of WND graph file:
  \code
  parent;child;relation_type
  \endcode
  
  Sample format of WND graph file:
  \code
  art;theatre;isa
  art;cinema;isa
  paranormal;occultism;isa
  paranormal;astrology;isa
  religion;theology;isa
  religion;roman_catholic;isa
  religion;mythology;isa
  graphic_arts;philately;isa
  plastic_arts;jewellery;isa
  plastic_arts;numismatics;isa
  ...
  \endcode
  
  WND could be download from \link http://wndomains.fbk.eu/index.html \endlink
  """
  def __init__(self, resources, options, str_name = 'WNDGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "WNDGraphBuilder"
    """
    super(WNDGraphBuilder, self).__init__(resources, options, str_name)

  def build_graph(self):
    """!
    Build WND graph. 

    Build graph based on WND graph file. Path to this file is set in config file. 
    Each node has an attribute named "wnd" which contain concept name, e.g. 
    religion. Whereas to each edge is assigned the relation type which occurs 
    between linked WND concepts.

    The directness of the graph and uniqueness of edges is set in the config file. 

    @return object of BaseGraph class
    """
    if not os.path.exists(self.resources().wnd_graph_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().wnd_graph_file())

    g = BaseGraph()
    g.init_graph(drctd = self.options().is_directed_graph())

    g.create_node_attribute("wnd", "string")
    g.create_edge_attribute("rel", "string")
    
    with open(self.resources().wnd_graph_file()) as wndgfile:
      for line in wndgfile:
        parent, child, rel = line.strip().split(';')[0:3]

        g.add_node(parent, [("wnd", parent)])
        g.add_node(child, [("wnd", child)])
        g.add_edge(parent, child, [("rel", rel)])

    if self.options().has_unique_edges():
      g.remove_edge_duplicates()

    return g
