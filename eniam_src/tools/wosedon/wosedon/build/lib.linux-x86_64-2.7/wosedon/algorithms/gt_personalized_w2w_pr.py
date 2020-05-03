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

class GTPersonalizedW2WPR(WSDAlgorithmInterface):
  """!
  Personalized Word-to-Word Page Rank algorithm based on the set of synsets 
  which correspond to lemmas from the analyzed context. Each word (lemma) from
  analyzed context is separately disambiguated. All synsets which belong to 
  lemmas from the analyzed context, excluding synsets which belong to 
  disambiguated word (lemma), have the same value in personalized vector "v", 
  which is equal to 1/N, where N is the number of synsets which do not 
  correspond to disambiguated word (lemma). Remaining synsets which do not
  belong to lemmas from the analyzed context and synsets which belong to 
  disambiguated word (lemma) have value equal to 0 in personalized vector "v".
  """

  def __init__(self, str_name = 'GTPersonalizedW2WPR'):
    """!
    @param str_name - default value same as the class name "GTPersonalizedW2WPR"
    """
    super(GTPersonalizedW2WPR, self).__init__(str_name)
    self._context_node_set = None

  def prepare_v(self, wsd_context, graph):
    """!
    Create personalized vector "v".

    All synsets which belong to lemmas from the analyzed context, excluding 
    synsets which belong to disambiguated word (lemma), have the same value 
    in personalized vector "v", which is equal to 1/N, where N is the number 
    of synsets which do not correspond to disambiguated word (lemma).

    Remaining synsets which do not belong to lemmas from the analyzed context
    and synsets which belong to disambiguated word (lemma) have value equal to
    0 in personalized vector "v".

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @return PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of personalized vector "v"
    """
    u = graph.use_graph_tool().new_vertex_property("double")
    for node in self._context_node_set:  # all others have 0.0 by default
      u[node.use_graph_tool()] = self._multiply_factor * (1.0 / float(len(self._context_node_set)))
      
    return u

  def run(self, wsd_context, graph, options, resources):
    """!
    Disambiguate analyzed context.

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @param options - object of AlgorithmOptions class
    @param resources - object of Resources class
    @return tuple of object of WSDRanking class and number of algorithm iterations
    """
    ret_iter_list = []

    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())

    for analyzed_lemma, nodes in lemma_on_node_dict.iteritems():
      self._context_node_set = set()
      self._set_context_node_set(analyzed_lemma, lemma_on_node_dict)

      pers_v = self.prepare_v(wsd_context, graph)
      (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                     pers = pers_v, 
                                     max_iter = 2 * options.max_iter(),
                                     damping = options.damping_factor(),
                                     ret_iter = True,
                                     weight = graph.use_graph_tool().ep["weight"])

      ret_iter_list.append(ret_iter)
      ranking = graph.ungraph_tool(ranking)
      wsd_rank.set_ranking_for_lemma(analyzed_lemma[0], 
                                     analyzed_lemma[1], 
                                     ranking, 
                                     create_new = False)

    return (wsd_rank, float(sum(ret_iter_list)) / float(len(ret_iter_list)))

  def _set_context_node_set(self, analyzed_lemma, lemma_on_node_dict):
    """!
    Create a set of synsets (precisely nodes from the graph) which belong
    to lemmas from the analyzed context, excluding synsets which belong to
    disambiguated word (lemma).

    @param analyzed_lemma - lemma which is actually disambiguated
    @param lemma_on_node_dict - dictionary where key is (lemma, pos_str) and
                                value is set of synsets (nodes from the graph)
                                belong to this lemma
    """
    self._context_node_set = set()
    for lemma, nodes in lemma_on_node_dict.iteritems():
      if lemma == analyzed_lemma:
        continue
      for node in nodes:
        self._context_node_set.add(node)
