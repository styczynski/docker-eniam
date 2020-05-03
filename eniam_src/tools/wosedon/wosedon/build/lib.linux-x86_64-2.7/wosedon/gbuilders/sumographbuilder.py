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

class SUMOGraphBuilder(GraphBuilderInterface):
  """!
  Class for building SUMO graph. 

  The built graph is based on Suggested Upper Merged Ontology (SUMO) graph file. 
  The Suggested Upper Merged Ontology (SUMO) is a formal upper and medium level 
  ontology including definitions of concepts and selected instance organised into 
  a network based on a few ontological relations:
    - subclass,
    - subrelation,
    - instance,
    - subAttribute.

  Format of SUMO graph file is presented below. First collumn in this file represent 
  parent name, second collumn represent child name, while last column represent 
  relation type between them.

  Format of SUMO graph file:
  \code
  parent;child;relation_type
  \endcode
  
  Sample format of SUMO graph file:
  \code
  AGM;AGM114;subclass
  AGM;AGM65;subclass
  WeatherFront;StationaryFront;subclass
  WeatherFront;WarmFront;subclass
  WeatherFront;OccludedFront;subclass
  WeatherFront;ColdFront;subclass
  ...
  \endcode
  
  SUMO ontology could be download from \link http://www.adampease.org/OP/ \endlink
  """
  def __init__(self, resources, options, str_name = 'SUMOGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "SUMOGraphBuilder"
    """
    super(SUMOGraphBuilder, self).__init__(resources, options, str_name)
  
  def build_graph(self):
    """!
    Build SUMO graph. 

    Build graph based on SUMO graph file. Path to this file is set in config file. 
    Each node has an attribute named "sumo" which contain concept name, e.g. 
    WeatherFront. Whereas to each edge is assigned the relation type which occurs 
    between linked SUMO concepts.

    The directness of the graph and uniqueness of edges is set in the config file. 

    @return object of BaseGraph class
    """
    if not os.path.exists(self.resources().sumo_graph_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().sumo_graph_file())

    g = BaseGraph()
    g.init_graph(drctd = self.options().is_directed_graph())

    g.create_node_attribute("sumo", "string")
    g.create_edge_attribute("rel", "string")

    with open(self.resources().sumo_graph_file()) as sgfile:
      for line in sgfile:
        parent, child, rel = line.strip().split(';')[0:3]

        g.add_node(parent, [("sumo", parent)])
        g.add_node(child, [("sumo", child)])
        g.add_edge(parent, child, [("rel", rel)])
    
    if self.options().has_unique_edges():
      g.remove_edge_duplicates()

    return g
