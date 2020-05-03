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

class CCLGraphBuilder(GraphBuilderInterface):
  """!
  Class for building graph extracted with CclExtractor.
  """
  
  def __init__(self, resources, options, str_name = 'CCLGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "CCLGraphBuilder"
    """
    super(CCLGraphBuilder, self).__init__(resources, options, str_name)
  
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
    if not os.path.exists(self.resources().ccl_graph_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().ccl_graph_file())

    g = BaseGraph()
    g.unpickle(self.resources().ccl_graph_file())
    g.set_directed(self.options().is_directed_graph())
    
    
    if self.options().has_unique_edges():
      g.remove_edge_duplicates()
    
    g.alias_edge_attribute('rel_type', 'rel')

    return g
