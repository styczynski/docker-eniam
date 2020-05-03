#!/usr/bin/env python
# -*- coding: utf-8 -*-

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

from graph_tool.all import *
from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.ranking.wsd_ranking import WSDRanking

class GTSUDOKURun2(WSDAlgorithmInterface):
  def __init__(self, str_name = 'GTSUDOKURun2'):
    super(GTSUDOKURun2, self).__init__(str_name)
    self._monosemy_set = None
    self._polysemy_dict = None
    self._subgraph = None
    self._main_nodes = None
    self._intermediate_nodes = None
    self._L = 2

  def prepare_v(self, wsd_context, graph, degree_of_polysemy):
    v = self._subgraph.new_vertex_property("double")
    for node in self._subgraph.vertices():
      old_node = self._subgraph.vp["old_node"][node]
      if old_node in self._monosemy_set:
        v[node] = self._multiply_factor
      else:
        v[node] = self._multiply_factor * (1.0 / float(degree_of_polysemy))
    return v
  
  def run(self, wsd_context, graph, options, resources = None):
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())

    self._set_monosemy_set_and_polysemy_dict(lemma_on_only_synset_node_dict)
    self._initialize_graph()

    for degree_of_polysemy in sorted(self._polysemy_dict):
      self._clear_all()
      self._add_monosemous_nodes()
      self._add_polysemous_nodes(degree_of_polysemy)
      self._build_subgraph(graph.use_graph_tool())

      pers_v = self.prepare_v(wsd_context, graph, degree_of_polysemy)
      (ranking, ret_iter) = pagerank(self._subgraph, 
                                     pers = pers_v, 
                                     max_iter = 2 * options.max_iter(),
                                     damping = options.damping_factor(),
                                     ret_iter = True)

      self._rebuild_monosemy_set(degree_of_polysemy, 
                                 lemma_on_only_synset_node_dict,
                                 ranking)

    new_ranking = graph.ungraph_tool(self._prepare_ranking(graph.use_graph_tool(), ranking))

    for (lemma, pos_str), nodes in lemma_on_only_synset_node_dict.iteritems():
      for node in nodes:
        node = int(node)
        if node in self._main_nodes:
          wsd_rank.set_ranking_for_lemma(lemma, pos_str, new_ranking)

    return (wsd_rank, ret_iter)

  def _set_monosemy_set_and_polysemy_dict(self, lemma_on_only_synset_node_dict):
    self._monosemy_set = set()
    self._polysemy_dict = defaultdict(set)
    for nodes in lemma_on_only_synset_node_dict.itervalues():
      synset_set_size = len(nodes)
      if synset_set_size == 1:
        self._monosemy_set.add(int(next(iter(nodes))))
      else:
        for node in nodes:
          self._polysemy_dict[synset_set_size].add(int(node))

  def _initialize_graph(self):
    self._subgraph = Graph(directed = False)
    
    old_node = self._subgraph.new_vertex_property("int")
    self._subgraph.vp["old_node"] = old_node

    self._main_nodes = {}
    self._intermediate_nodes = {}

  def _add_monosemous_nodes(self):
    for node in self._monosemy_set:
      if node not in self._main_nodes:
        v = self._subgraph.add_vertex()
        self._subgraph.vp["old_node"][v] = node
        self._main_nodes[node] = v

  def _add_polysemous_nodes(self, degree_of_polysemy):
    for node in self._polysemy_dict[degree_of_polysemy]:
      if node not in self._main_nodes:
        v = self._subgraph.add_vertex()
        self._subgraph.vp["old_node"][v] = node
        self._main_nodes[node] = v

  def _get_new_node(self, old_node, stack):
    new_node = None
    if old_node in self._main_nodes:
      new_node = self._main_nodes[old_node]
    elif old_node in self._intermediate_nodes:
      new_node = self._intermediate_nodes[old_node]
    else:
      v = self._subgraph.add_vertex()
      self._subgraph.vp["old_node"][v] = old_node
      self._intermediate_nodes[old_node] = v
      new_node = v
      stack.append(old_node)
    return new_node

  def _build_subgraph(self, graph):
    stack = self._main_nodes.keys()
    while stack:
      old_node = stack.pop()
      for path in self._dfs_paths(graph.vertex(old_node)):
        for i in range(0, len(path)-1):
          parent_node = self._get_new_node(int(path[i]), stack)
          child_node = self._get_new_node(int(path[i+1]), stack)
          self._subgraph.add_edge(parent_node, child_node)
    remove_parallel_edges(self._subgraph)

  def _dfs_paths(self, v_start):
    stack = [(v_start, [v_start])]
    while stack:
      (vertex, path) = stack.pop()
      if len(path) <= self._L:
        for ngb in vertex.all_neighbours():
          if ngb not in path:
            if int(ngb) in self._main_nodes:
              yield path + [ngb]
            else:
              stack.append((ngb, path + [ngb]))

  def _rebuild_monosemy_set(self, degree_of_polysemy, lemma_on_only_synset_node_dict, ranking):
    for nodes in lemma_on_only_synset_node_dict.itervalues():
      synset_set_size = len(nodes)
      if synset_set_size == degree_of_polysemy:
        best_rank = 0.0
        best_nodes = set()
        for node in nodes:
          if int(node) not in self._main_nodes:
            continue
          new_node = self._main_nodes[int(node)]
          rank = ranking[new_node]
          if rank > best_rank:
            best_rank = rank
            best_nodes.clear()
            best_nodes.add(new_node)
          elif rank == best_rank:
            best_nodes.add(new_node)
        if len(best_nodes) == 1:
          best_node = next(iter(best_nodes))
          self._monosemy_set.add(self._subgraph.vp["old_node"][best_node])

  def _clear_all(self):
    self._subgraph.clear()
    self._main_nodes.clear()
    self._intermediate_nodes.clear()

  def _prepare_ranking(self, graph, ranking):
    new_ranking = graph.new_vertex_property("double")
    for node in self._subgraph.vertices():
      old_node = self._subgraph.vp["old_node"][node]
      new_ranking[graph.vertex(old_node)] = ranking[node]
    return new_ranking
