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

class GTPersonalizedW2WPRNorm(WSDAlgorithmInterface):
  """!
  Personalized Word-to-Word Page Rank algorithm based on the set of synsets 
  which correspond to lemmas from the analyzed context. Values in personalized
  vector "v" which corespond to all synsets which belong to lemmas from 
  the analyzed context depends on the number of lemmas meanings. Each word
  (lemma) from analyzed context is separately disambiguated. 

  Synsets which belong to word (lemma) which is actually disambiguated
  have value equal to 0 in personalized vector "v". While rest of synsets which
  correspond to words (lemmas) from the analyzed context have value
  calculated as describe below.

  If lemma has N meanings (synsets) then values in personalized vector 
  "v" for all its meanings (synsets) will be 1/N. If this same synset
  occur in two lemmas from the analyzed context then boths values are added.

  For example:

  Suppose that lemma "kot" is actually disambiguated.

  \code
  Lemma    Set of synsets for lemma
  kot      1, 2, 3, 4, 5
  pies     5, 6, 7

  Personalized vector "v":
  Synset ID             Value
  1 (kot-1)             0
  2 (kot-2)             0
  3 (kot-3)             0
  4 (kot-4)             0
  5 (kot-5) & (pies-1)  0 + 1/3
  6 (pies-2)            1/3
  7 (pies-3)            1/3
  \endcode

  Remaining synsets which do not belong to lemmas from the analyzed context
  have value equal to 0 in personalized vector "v".
  """

  def __init__(self, str_name = 'GTPersonalizedW2WPRNorm'):
    """!
    @param str_name - default value same as the class name "GTPersonalizedW2WPRNorm"
    """
    super(GTPersonalizedW2WPRNorm, self).__init__(str_name)
    self._context_node_dict = None

  def prepare_v(self, wsd_context, graph):
    """!
    Create personalized vector "v".

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @return PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of personalized vector "v"
    """
    
    u = graph.use_graph_tool().new_vertex_property("double")
    for node in self._context_node_dict:  # all others have 0.0 by default
      u[node.use_graph_tool()] = self._multiply_factor * self._context_node_dict[node]
    
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
      self._set_context_node_dict(analyzed_lemma, lemma_on_node_dict)

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

  def _set_context_node_dict(self, analyzed_lemma, lemma_on_node_dict):
    """!
    Create a dictionary where key is object of node from the graph which
    corespond to the synset which belong to lemma from the analyzed context,
    excluding synsets which belong to disambiguated word (lemma).
    Whereas value is a value which synset from the key will be have in
    personalized vector "v".

    @param analyzed_lemma - lemma which is actually disambiguated
    @param lemma_on_node_dict - dictionary where key is (lemma, pos_str) and
                                value is set of synsets (nodes from the graph)
                                belong to this lemma
    """
    self._context_node_dict = {}
    for lemma, nodes in lemma_on_node_dict.iteritems():
      if lemma == analyzed_lemma:
        continue
      synset_set_size = float(len(nodes))
      for node in nodes:
        if self._context_node_dict.has_key(node):
          self._context_node_dict[node] += 1.0 / synset_set_size
        else:
          self._context_node_dict[node] = 1.0 / synset_set_size
