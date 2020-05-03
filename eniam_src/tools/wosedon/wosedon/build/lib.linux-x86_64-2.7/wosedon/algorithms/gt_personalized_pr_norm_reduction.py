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

import math

from graph_tool.centrality import pagerank
from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.ranking.wsd_ranking import WSDRanking

class GTPersonalizedPRNormReduction(WSDAlgorithmInterface):
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
  have value equal to 0 in personalized vector "v"..

  Additionally after completing a given number of iterations, the percent of
  worst nodes which correspond separately to each lemma from analyzed context
  are removed from graph and disambiguation process is executed all over again.
  For example, we set percent of worst nodes to 50%. For lemma "kot" we have
  five synset, namely 1, 2, 3, 4, 5. After disambiguation process it turned
  out that the 50% of worst synsets are 1, 4 and 5. Thus we remove from graph
  nodes which correspond to the worst synsets and execute disambiguation
  process all over again.
  """

  def __init__(self, str_name = 'GTPersonalizedPRNormReduction'):
    """!
    @param str_name - default value same as the class name "GTPersonalizedPRNormReduction"
    """
    super(GTPersonalizedPRNormReduction, self).__init__(str_name)
    self._context_node_dict = None

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
    percent_of_rank = 50
    ret_iter_list = []

    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())
    self._set_context_node_dict(lemma_on_node_dict)
    
    pers_v = self.prepare_v(wsd_context, graph)
    (first_ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                             pers = pers_v, 
                             max_iter = 2 * options.max_iter(),
                             damping = options.damping_factor(),
                             ret_iter = True,
                             weight = graph.use_graph_tool().ep["weight"])
    ret_iter_list.append(ret_iter)

    for (lemma, pos_str), nodes in lemma_on_only_synset_node_dict.iteritems():
      ranking = first_ranking

      if len(nodes) == 1:
        ranking = graph.ungraph_tool(ranking)
        wsd_rank.set_ranking_for_lemma(lemma, 
                                       pos_str, 
                                       ranking, 
                                       create_new = True)
        continue      
      
      # Kopia slownika
      lemma_on_node_dict_copy = {}
      for k, v in lemma_on_node_dict.iteritems():
        lemma_on_node_dict_copy[k] = set()
        for v_el in v:
          lemma_on_node_dict_copy[k].add(v_el)

      # Liczba synsetow do usuniecia dla zadanego procentu
      # Jezeli liczba synsetow jest niepazysta to zaokraglane jest w gore
      number_of_selected_syn = int(math.ceil(float(len(nodes)) / 100.0 * float(percent_of_rank)))

      # Zbior wierzcholkow do odfiltrowania
      nodes_to_filter_set = set()

      # Proces ujednoznaczniania z usuwaniem wezlow
      for i in range(number_of_selected_syn):
        # Szukanie wierzcholka z najnizszym rankingiem
        worst_node = None
        worst_rank = float("inf")
        for node in lemma_on_node_dict_copy[(lemma, pos_str)]:
          if ranking[node] < worst_rank:
            worst_node = node
            worst_rank = ranking[node]
        nodes_to_filter_set.add(worst_node)

        # Usuniecie wierzcholka z najnizszym rankingiem z kopi slownika
        lemma_on_node_dict_copy[(lemma, pos_str)].remove(worst_node)
        self._set_context_node_dict(lemma_on_node_dict_copy)
        # Odfiltrowanie wierzcholka z najnizszym rankingiem z grafu
        node_filter = graph.use_graph_tool().new_vertex_property("bool")
        for node in nodes_to_filter_set:
          node_filter[node] = True
        graph.use_graph_tool().set_vertex_filter(node_filter, True)

        pers_v = self.prepare_v(wsd_context, graph)
        (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                       pers = pers_v, 
                                       max_iter = 2 * options.max_iter(),
                                       damping = options.damping_factor(),
                                       ret_iter = True,
                                       weight = graph.use_graph_tool().ep["weight"])
        ret_iter_list.append(ret_iter)

      # Wyczszczenie grafu do stanu poczatkowego
      graph.use_graph_tool().clear_filters()
      # Przywrocenie _context_node_dict do stanu poczatkowego
      self._set_context_node_dict(lemma_on_node_dict)
      
      ranking = graph.ungraph_tool(ranking)
      wsd_rank.set_ranking_for_lemma(lemma, 
                                     pos_str, 
                                     ranking, 
                                     create_new = True)

    return (wsd_rank, float(sum(ret_iter_list)) / float(len(ret_iter_list)))

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
