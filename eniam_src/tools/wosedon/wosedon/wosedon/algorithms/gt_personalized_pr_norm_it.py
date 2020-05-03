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

class GTPersonalizedPRNormIt(WSDAlgorithmInterface):
  """!
  Personalized Page Rank algorithm based on the set of synsets which
  correspond to lemmas from the analyzed context. Values in personalized
  vector "v" which corespond to all synsets which belong to lemmas from 
  the analyzed context depends on the number of lemmas meanings.

  If lemma has N meanings (synsets) then values in personalized vector 
  "v" for all its meanings (synsets) will be 1/N. If this same synset
  occur in two lemmas from the analyzed context then boths values are added.

  For example:
  \code
  Lemma    Set of synsets for lemma
  kot      1, 2, 3, 4, 5
  pies     5, 6, 7

  Personalized vector "v":
  Synset ID             Value
  1 (kot-1)             1/5
  2 (kot-2)             1/5
  3 (kot-3)             1/5
  4 (kot-4)             1/5
  5 (kot-5) & (pies-1)  1/5 + 1/3
  6 (pies-2)            1/3
  7 (pies-3)            1/3
  \endcode

  Remaining synsets which do not belong to lemmas from the analyzed context
  have value equal to 0 in personalized vector "v".

  Additionally after each iteration values which corespond to lemmas from the
  analyzed context in personalized vector "v" are normalized. Firstly all values
  from personalized vector "v", for only this synsets which belong to one lemma,
  are summed and the new value for this synsets are calculated as division old
  value over sum.

  For example:
  \code
  Synset ID             Old value    New value
  1 (kot-1)             848.29       0.20
  2 (kot-2)             12.12        0.003
  3 (kot-3)             2382.12      0.56
  4 (kot-4)             1000.22      0.24
  5 (kot-5) & (pies-1)  12.11        0.00002
  6 (pies-2)            0.12         0.001
  7 (pies-3)            123.11       0.99
  \endcode
  """

  def __init__(self, str_name = 'GTPersonalizedPRNormIt'):
    """!
    @param str_name - default value same as the class name "GTPersonalizedPRNormIt"
    """
    super(GTPersonalizedPRNormIt, self).__init__(str_name)
    self._context_node_dict = None
  
  def _normalize_gt_ranking(self, wsd_context, g, ranking, lemma_on_node_dict):
    """!
    Normalize Page Rank ranking.

    @param wsd_context - object of WSDContext class
    @param g - Graph object from graph_tool library
    @param ranking - PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of ranking values 
    @param lemma_on_node_dict - dictionary where key is lemma and value is
                                set of synsets (nodes from the graph) belong
                                to this lemma
    @return PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of new ranking values
    """
    l_pos = set([
      (wsd_context.get_token_lemma_str(t),
       wsd_context.get_token_coarse_pos(t)) for t in wsd_context.tokens()])
      
    for lemma, pos_str in l_pos:
      s = 0
      for vsyn in lemma_on_node_dict[(lemma, pos_str)]:
        s += ranking[vsyn]
      for vsyn in lemma_on_node_dict[(lemma, pos_str)]:
        ranking[vsyn] /= s
    return ranking

  def prepare_v(self, wsd_context, graph):
    """!
    Create personalized vector "v".

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @return PropertyMap (see: http://graph-tool.skewed.de/static/doc/graph_tool.html?highlight=new_vertex_property#graph_tool.Graph.new_vertex_property) of personalized vector "v"
    """
    v = graph.use_graph_tool().new_vertex_property("double")
    for node in self._context_node_dict:
      v[node.use_graph_tool()] = self._multiply_factor * self._context_node_dict[node]
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
    self._set_context_node_dict(lemma_on_node_dict)

    pers_v = self.prepare_v(wsd_context, graph)
    ranking = None
    for i in range(0, options.max_iter()):
      (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                   pers = pers_v,
                                   prop = ranking,
                                   max_iter = 2,
                                   damping = options.damping_factor(),
                                   ret_iter = True,
                                   weight = graph.use_graph_tool().ep["weight"])
      ranking = self._normalize_gt_ranking(wsd_context, graph.use_graph_tool(), ranking, lemma_on_node_dict)
    
    ranking = graph.ungraph_tool(ranking)
    for (lemma, pos_str) in lemma_on_only_synset_node_dict.iterkeys():
      wsd_rank.set_ranking_for_lemma(lemma, pos_str, ranking)

    return (wsd_rank, ret_iter)

  def _set_context_node_dict(self, lemma_on_node_dict):
    """!
    Create a dictionary where key is object of node from the graph which
    corespond to the synset which belong to lemma from the analyzed context.
    Whereas value is a value which synset from the key will be have in
    personalized vector "v".

    @param lemma_on_node_dict - dictionary where key is (lemma, pos_str) and
                                value is set of synsets (nodes from the graph)
                                belong to this lemma
    """
    self._context_node_dict = {}
    for nodes in lemma_on_node_dict.itervalues():
      synset_set_size = float(len(nodes))
      for node in nodes:
        if self._context_node_dict.has_key(node):
          self._context_node_dict[node] += 1.0 / synset_set_size
        else:
          self._context_node_dict[node] = 1.0 / synset_set_size
