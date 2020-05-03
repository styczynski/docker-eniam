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

from graph_tool.centrality import pagerank
from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.ranking.wsd_ranking import WSDRanking

class GTStaticPR(WSDAlgorithmInterface):
  """!
  Static Page Rank is a abbreviation from GraphToolStatic PageRank. 
  In this imeplementation, Graph tool Page Rank is used.
  This is Static Page Rank so no personalization is done.
  """
  
  def __init__(self, str_name = 'GTStaticPR'):
    """!
    @param str_name - default value same as the class name "GTStaticPR"
    """
    super(GTStaticPR, self).__init__(str_name)

  def prepare_v(self, wsd_context, graph):
    """!
    Personalization vector "v" is made for all nodes into graph.
    All nodes has same value equal to 1/N where N is the number of
    nodes into Graph.

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @return PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of personalized vector "v"
    """
    w = (1.0 / float(graph.use_graph_tool().num_vertices())) * self._multiply_factor

    v = graph.use_graph_tool().new_vertex_property("double")
    for node in graph.use_graph_tool().vertices():
      v[node] = w
    # v.set_value(w)
    
    return v
  
  def run(self, wsd_context, graph, options, resources):
    """!
    Disambiguate analyzed context.

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @param options - object of AlgorithmOptions class
    @param resources - object of Resources class
    @return tuple of object of WSDRanking class and number of algorithm iterations
    """
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())

    pers_v = self.prepare_v(wsd_context, graph)
    (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                   pers = pers_v, 
                                   max_iter = 2 * options.max_iter(),
                                   damping = options.damping_factor(),
                                   ret_iter = True,
                                   weight = graph.use_graph_tool().ep["weight"])
    
    ranking = graph.ungraph_tool(ranking)
    for (lemma, pos_str) in lemma_on_only_synset_node_dict.iterkeys():
      wsd_rank.set_ranking_for_lemma(lemma, pos_str, ranking)

    return (wsd_rank, ret_iter)
