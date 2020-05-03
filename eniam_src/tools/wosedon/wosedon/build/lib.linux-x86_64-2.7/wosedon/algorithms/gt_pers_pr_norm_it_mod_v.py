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

# Normalizacja dla lematow w wektorze inicjujacym do <0,1> dla synsetow lematu
# Zmiana wektora personalizacyjnego co iteracje na wynik pr pomnozony przez 
# poczatkowy pr. W kazdej iteracji wektor persnalizacyjny normalizowany
# jest dla <0,1>
class GTPersPRNormItModV(WSDAlgorithmInterface):
  def __init__(self, str_name = 'GTPersPRNormItModV'):
    super(GTPersPRNormItModV, self).__init__(str_name)
    self._context_node_dict = None

  def prepare_v(self, wsd_context, graph):
    v = graph.use_graph_tool().new_vertex_property("double")
    for node in self._context_node_dict:
      v[node.use_graph_tool()] = self._multiply_factor * self._context_node_dict[node]
    return v

  def prepare_new_v(self, pers_v, ranking, graph):
    v = graph.use_graph_tool().new_vertex_property("double")
    if not ranking:
      for node in graph.all_nodes():
        if self._context_node_dict.has_key(node):
          v[node.use_graph_tool()] = self._multiply_factor * self._context_node_dict[node]
        else:
          v[node.use_graph_tool()] = 0.0
    else:
      for node in graph.all_nodes():
        v[node.use_graph_tool()] = (pers_v[node.use_graph_tool()] + ranking[node.use_graph_tool()])
    return v

  def _normalize_gt_ranking(self, wsd_context, g, ranking, lemma_on_node_dict):
    # TODO: Moze to warto dodac do wsd_context, metode typu: lemmas_poses()?
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
  
  def run(self, wsd_context, graph, options, resources):
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())
    self._set_context_node_dict(lemma_on_node_dict)

    pers_v = self.prepare_v(wsd_context, graph)
    ranking = None
    for i in range(0, options.max_iter()):
      # Prepare the new personalized vector which is multiplication of
      # the ranking from previous loop with personalized page rank given 
      # on the begining of the algorithm
      m_pers_v = self.prepare_new_v(pers_v, ranking, graph)

      # In each loop the personalized vector is normalized for each lemma
      # in the boundary of 0 to 1.
      m_pers_v = self._normalize_gt_ranking(wsd_context, graph.use_graph_tool(), m_pers_v, lemma_on_node_dict)
      (ranking, ret_iter) = pagerank(graph.use_graph_tool(), 
                                   pers = m_pers_v,
                                   prop = ranking,
                                   max_iter = 2,
                                   damping = options.damping_factor(),
                                   ret_iter = True,
                                   weight = graph.use_graph_tool().ep["weight"])
    
    ranking = graph.ungraph_tool(ranking)
    
    for (lemma, pos_str) in lemma_on_only_synset_node_dict.iterkeys():
      wsd_rank.set_ranking_for_lemma(lemma, pos_str, ranking)

    return (wsd_rank, ret_iter)

  def _set_context_node_dict(self, lemma_on_node_dict):
    self._context_node_dict = {}
    for nodes in lemma_on_node_dict.itervalues():
      synset_set_size = float(len(nodes))
      for node in nodes:
        if self._context_node_dict.has_key(node):
          self._context_node_dict[node] += 1.0 / synset_set_size
        else:
          self._context_node_dict[node] = 1.0 / synset_set_size
        
