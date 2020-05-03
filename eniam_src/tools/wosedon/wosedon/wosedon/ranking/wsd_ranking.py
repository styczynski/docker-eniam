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

from collections import defaultdict
import logging
import sys

class WSDRanking():
  def __init__(self):
    self._ranking_for_lemma_dict = {}
    self._ctx_ranking = None
    self._lemma_on_node_dict = None
    self._lemma_on_only_synset_node_dict = None


  def set_ranking_for_context(self, wsd_context, graph):
    """
    Sets internal variable _ctx_ranking. It will become a list of tokens, each
    of which will be assigned the ranking of its lemma. It will be new, pure
    ranking, free off unrelated nodes (cf. set_ranking_for_lemma()).
    """
    lon, lon_only_synset = self.get_lemma_on_node_dict(wsd_context, graph)

    rank_for_context_dict = defaultdict(list)
    for (lemma, pos_str), nodes in lon_only_synset.iteritems():
      ranking = self.get_ranking_for_lemma(lemma, pos_str)
      if not ranking:
        continue
      for node in nodes:
        rank_for_context_dict[(lemma, pos_str)].append((node, ranking[node]))
      all_rank = rank_for_context_dict[(lemma, pos_str)]
      sorted_all_rank = sorted(all_rank, key = lambda rank: rank[1], reverse = True)
      rank_for_context_dict[(lemma, pos_str)] = sorted_all_rank

    rank_for_tokens_list = []
    for t in wsd_context.tokens():
      lemma = wsd_context.get_token_lemma_str(t).lower()
      #lemma = wsd_context.get_token_lemma_str(t)
      pos_str = wsd_context.get_token_coarse_pos(t)
      if rank_for_context_dict.has_key((lemma, pos_str)):
        rank = rank_for_context_dict[(lemma, pos_str)]
        rank_for_tokens_list.append((t, rank))
      else:
        rank_for_tokens_list.append((t, None))

    self._ctx_ranking = rank_for_tokens_list

  def get_ranking_for_context(self, wsd_context, graph):
    """
    Returns a list [(token, its ranking) for each token in context].
    """
    self.set_ranking_for_context(wsd_context, graph)
    return self._ctx_ranking


  def set_ranking_for_lemma(self, lemma, pos_str, ranking, create_new = False):
    """
    Sets a ranking that will be used for that lemma. Ranking is a mapping from
    nodes to floats. Such ranking may contain nodes absolutely unconnected with
    the lemma, WSDRanking is able to ignore them.
    """
    if create_new:
      self._ranking_for_lemma_dict[(lemma, pos_str)] = ranking.copy()
    else:
      self._ranking_for_lemma_dict[(lemma, pos_str)] = ranking

  def get_ranking_for_lemma(self, lemma, pos_str):
    """
    Returns previoulsly set ranking for the lemma.
    """
    ranking = None
    if self._ranking_for_lemma_dict.has_key((lemma, pos_str)):
      ranking = self._ranking_for_lemma_dict[(lemma, pos_str)]
    else:
      logging.getLogger(__name__).warning('There is no ranking for lemma "%s".', str(lemma))
    return ranking


  def _get_context_lemma_set(self, wsd_context):
    context_lemma_set = set()
    for t in wsd_context.tokens():
      lemma = wsd_context.get_token_lemma_str(t).lower()
      #lemma = wsd_context.get_token_lemma_str(t)
      pos_str = wsd_context.get_token_coarse_pos(t)
      context_lemma_set.add((lemma, pos_str))
    return context_lemma_set

  def _set_lemma_on_node_dict(self, wsd_context, graph, ini_nodes=None):
    context_lemma_set = self._get_context_lemma_set(wsd_context)
    self._lemma_on_node_dict = defaultdict(set)
    self._lemma_on_only_synset_node_dict = defaultdict(set)
    if not ini_nodes:
      for node in graph.all_nodes():
        if node.synset:
          self._set_synset_node(graph, 
                                node, 
                                wsd_context, 
                                context_lemma_set)
    else:
      for node in graph.all_nodes():
        if node.synset:
          self._set_synset_node(graph, 
                                node, 
                                wsd_context, 
                                context_lemma_set, 
                                'synset' in ini_nodes)
        elif 'sumo' in ini_nodes and node.sumo:
          self._set_not_synset_node(graph, 
                                    node, 
                                    wsd_context, 
                                    context_lemma_set)
        elif 'wnd' in ini_nodes and node.wnd:
          self._set_not_synset_node(graph, 
                                    node, 
                                    wsd_context, 
                                    context_lemma_set)
        elif 'msr' in ini_nodes and node.msr:
          self._set_not_synset_node(graph, 
                                    node, 
                                    wsd_context, 
                                    context_lemma_set)

  def _set_synset_node(self, graph, node, wsd_context, context_lemma_set, 
                       synset_in_ini_nodes=True):
    lemma_and_pos_str_set = self._get_lemma_and_pos_str_set(graph, 
                                                            node, 
                                                            wsd_context, 
                                                            context_lemma_set)
    if lemma_and_pos_str_set:
      for (lemma, pos_str) in lemma_and_pos_str_set:
        if synset_in_ini_nodes:
          self._lemma_on_node_dict[(lemma, pos_str)].add(node)
        self._lemma_on_only_synset_node_dict[(lemma, pos_str)].add(node)

  def _set_not_synset_node(self, graph, node, wsd_context, context_lemma_set):
    for ngb in node.all_neighbours():
      if ngb.synset:
        lemma_and_pos_str_set = self._get_lemma_and_pos_str_set(graph, 
                                                                ngb, 
                                                                wsd_context, 
                                                                context_lemma_set)
        if lemma_and_pos_str_set:
          for (lemma, pos_str) in lemma_and_pos_str_set:
            self._lemma_on_node_dict[(lemma, pos_str)].add(node)

  def _get_lemma_and_pos_str_set(self, graph, node, wsd_context, context_lemma_set):
    lemma_and_pos_str_set = set()
    lu_set = node.synset.lu_set
    for lu in lu_set:
      lemma = lu.lemma.lower()
      #lemma = lu.lemma
      pos_str = wsd_context.convert_num_pos_to_coarse_str(lu.pos)
      if (lemma, pos_str) in context_lemma_set:
        lemma_and_pos_str_set.add((lemma, pos_str))
    return lemma_and_pos_str_set

  def get_lemma_on_node_dict(self, wsd_context, graph, ini_nodes=None):
    """
    Returns two dictionaries. Both map each lemma from context to corresponding
    nodes.
    
    If ini_nodes is not set, both will acknowledge all synset nodes and only them.
    Otherwise in the first dictionary there will be nodes of types listed in ini_nodes,
    and only they. Whereas the second dict will use synset nodes and nothing more.
    
    """
    if not self._lemma_on_node_dict:
      self._set_lemma_on_node_dict(wsd_context, graph, ini_nodes)
    return self._lemma_on_node_dict, self._lemma_on_only_synset_node_dict
