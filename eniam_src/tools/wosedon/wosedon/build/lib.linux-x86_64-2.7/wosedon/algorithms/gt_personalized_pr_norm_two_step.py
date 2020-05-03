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

class GTPersonalizedPRNormTwoStep(WSDAlgorithmInterface):
  """!
  Two-step Word Sense Disambiguation.

  The disambiguation process was divided into two steps with running Page Rank
  into both.

  This method used one big graph, which are build by merging e.g. SUMO graph and
  plWordNet graph.

  In the first step (called as coarse grained disambiguation), e.g. SUMO ontology
  is utilised as a network of interconnected concepts. For each word "w" from
  the context, we choose the set of concepts connected with "w" and initialise
  personalized vector "v". Then, the process of coarse disambiguation is run.
  After the disambiguation, for word "w" we choose only one, the best concept.

  The second phase of disambiguation (fine-grained disambiguation) based on
  plWordNet graph and synset mappings onto SUMO ontology. The difference is
  in "v" initialisation. Only these elements are initialised, which has
  connections (mappings) with concept chosen in previous step. For example,
  for word "zamek", in the previous stage the "Lock" concept has been assigned,
  only two from six synsets are initialised: {zamek 2 'lock' (wytw)} and
  {zamek 5 'semaphore' (wytw)}. Next, the disambiguation process is run
  and as the result the ranking value for each node is returned. For each word,
  the synset with the highest ranking value is chosen as a meaning of words.

  Initialization process of personalized vector "v" was described below.

  If lemma has N meanings (synsets/concepts) then values in personalized vector 
  "v" for all its meanings (synsets/concepts) will be 1/N. If this same synset
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
  """

  def __init__(self, str_name = 'GTPersonalizedPRNormTwoStep'):
    """!
    @param str_name - default value same as the class name "GTPersonalizedPRNormTwoStep"
    """
    super(GTPersonalizedPRNormTwoStep, self).__init__(str_name)
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
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())

    # Pierwszy etap ujednoznaczniania:
    # Wazne jest aby w pliku konfiguracyjnym ustawic parametr ini_nodes = sumo.
    self._set_context_node_dict_for_first_step(lemma_on_node_dict)

    pers_v = self.prepare_v(wsd_context, graph)
    (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                   pers = pers_v, 
                                   max_iter = 2 * options.max_iter(),
                                   damping = options.damping_factor(),
                                   ret_iter = True,
                                   weight = graph.use_graph_tool().ep["weight"])

    best_concept_for_lemma_dict = {}
    for (lemma, pos_str) in lemma_on_only_synset_node_dict.iterkeys():
      if (lemma, pos_str) in lemma_on_node_dict:
        nodes = lemma_on_node_dict[(lemma, pos_str)]
        best_node = None
        best_ranking = 0.0
        for node in nodes:
          if best_ranking < ranking[node.use_graph_tool()]:
            best_node = node
            best_ranking = ranking[node.use_graph_tool()]
        best_concept_for_lemma_dict[(lemma, pos_str)] = best_node
      else:
        best_concept_for_lemma_dict[(lemma, pos_str)] = None

    # Drugi etap ujednoznaczniania:
    self._set_context_node_dict_for_second_step(best_concept_for_lemma_dict,
                                                lemma_on_only_synset_node_dict,
                                                graph)

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

  def _set_context_node_dict(self, nodes_set):
    """!
    Create a dictionary where key is object of node from the graph which
    corespond to the synset which belong to lemma from the analyzed context.
    Whereas value is a value which synset from the key will be have in
    personalized vector "v".

    @param nodes_set - set of nodes from the graph
    """
    nodes_set_size = float(len(nodes_set))
    for node in nodes_set:
      if self._context_node_dict.has_key(node):
        self._context_node_dict[node] += 1.0 / nodes_set_size
      else:
        self._context_node_dict[node] = 1.0 / nodes_set_size

  def _set_context_node_dict_for_first_step(self, lemma_on_node_dict):
    """!
    Create a dictionary where key is object of node from the graph which
    corespond to the synset which belong to lemma from the analyzed context.
    Whereas value is a value which synset from the key will be have in
    personalized vector "v" for the first step of disambiguation process.

    @param lemma_on_node_dict - dictionary where key is (lemma, pos_str) and
                                value is set of synsets (nodes from the graph)
                                belong to this lemma
    """
    self._context_node_dict = {}
    for nodes in lemma_on_node_dict.itervalues():
      self._set_context_node_dict(nodes)

  def _set_context_node_dict_for_second_step(self, 
                                             best_concept_for_lemma_dict,
                                             lemma_on_only_synset_node_dict,
                                             graph):
    """!
    Create a dictionary where key is object of node from the graph which
    corespond to the synset which belong to lemma from the analyzed context.
    Whereas value is a value which synset from the key will be have in
    personalized vector "v" for the second step of disambiguation process.

    @param best_concept_for_lemma_dict - dictionary where key is (lemma, pos_str)
                                         and value is node which corespond to the
                                         best concept chosen in the first stage
                                         of word sense disambiguation
    @param lemma_on_only_synset_node_dict - dictionary where key is (lemma, pos_str)
                                            and value is set of synsets (nodes
                                            from the graph) belong to this lemma
    """
    self._context_node_dict = {}
    for (lemma, pos_str), node in best_concept_for_lemma_dict.iteritems():
      synsets_nodes_set = set()
      synsets_nodes_set = lemma_on_only_synset_node_dict[(lemma, pos_str)]
      if node:
        chosen_synsets_nodes_set = set()
        for ngb in node.all_neighbours():
          if ngb.synset:
            if ngb in synsets_nodes_set:
              chosen_synsets_nodes_set.add(ngb)
        self._set_context_node_dict(chosen_synsets_nodes_set)
      else:
        self._set_context_node_dict(synsets_nodes_set)
