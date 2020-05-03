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
from graph_tool.all import *
from graph_tool.draw import graph_draw
from graph_tool.topology import shortest_path

import matplotlib.pyplot
from collections import defaultdict

class Visualisation():
  def __init__(self):
    pass

  def make_visualisation(self, wsd_context, graph, wsd_rank, v_dir):
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph)

    nodes_from_context_list = set()
    for node_set in lemma_on_only_synset_node_dict.itervalues():
      nodes_from_context_list = nodes_from_context_list.union(node_set)
    nodes_from_context_list = list(nodes_from_context_list)

    for t in wsd_context.tokens():
      lemma = wsd_context.get_token_lemma_str(t)
      pos_str = wsd_context.get_token_coarse_pos(t)
      ranking = wsd_rank.get_ranking_for_lemma(lemma, pos_str)
      if not ranking:
        continue

      v_graph = graph.G().copy()

      # PageRank dla wierzcholka
      node_attr = v_graph.new_vertex_property("double")
      v_graph.vp["PageRank"] = node_attr
      # PropertyMap do oznaczenia czy dany synset jest
      # synsetem z kontekstu
      node_attr = v_graph.new_vertex_property("bool")
      v_graph.vp["context_synset"] = node_attr
      # Etykieta wierzcholka
      node_attr = v_graph.new_vertex_property("string")
      v_graph.vp["vertex_text"] = node_attr
      # Kolor etykiety wierzcholka
      node_attr = v_graph.new_vertex_property("string")
      v_graph.vp["vertex_text_color"] = node_attr
      # Kolor wypelnienia wierzcholka
      node_attr = v_graph.new_vertex_property("vector<double>")
      v_graph.vp["vertex_fill_color"] = node_attr
      # Pozycje wezlow
      node_attr = v_graph.new_vertex_property("vector<double>")
      v_graph.vp["pos"] = node_attr

      nodes_not_to_filter_set = set()
      for i in range(len(nodes_from_context_list)-1):
        source = nodes_from_context_list[i]
        #v_graph.vp["context_synset"][source] = True
        for j in range(1, len(nodes_from_context_list)):
          target = nodes_from_context_list[j]
          #v_graph.vp["context_synset"][target] = True
          nlist, elist = shortest_path(v_graph, source, target)
          for n in nlist:
            nodes_not_to_filter_set.add(n)
            v_graph.vp["PageRank"][n] = ranking[n]

      sorted_pagerank_set = sorted(set(v_graph.vp["PageRank"].a))
      color_for_ranking_dict = {}
      for i, rank in enumerate(sorted_pagerank_set):
        color_for_ranking_dict[rank] = float(i) / float((len(sorted_pagerank_set) - 1))

      for node in lemma_on_only_synset_node_dict[(lemma, pos_str)]:
        v_graph.vp["context_synset"][node] = True

      # Odfiltrowanie wierzcholkow, ktore nie znalazly sie
      # na sciezkach pomiedzy wierzcholkami z kontekstu
      node_filter = v_graph.new_vertex_property("bool")
      for n in v_graph.vertices():
        if n in nodes_not_to_filter_set:
          node_filter[n] = True
        else:
          node_filter[n] = False
      v_graph.set_vertex_filter(node_filter)
      v_graph.purge_vertices()

      nouns_nodes_not_to_filter_set = set()
      verbs_nodes_not_to_filter_set = set()
      for n in v_graph.vertices():
        for lu in v_graph.vp["synset"][n].lu_set:
          # Dodanie wezla grafu do odpowiedniego dla
          # czesci mowy zbioru
          if lu.pos == 2:
            nouns_nodes_not_to_filter_set.add(n)
          elif lu.pos == 1:
            verbs_nodes_not_to_filter_set.add(n)
          # Ustawienie etykiety dla kazdego wezla
          vertex_text = lu.lemma + '-' + str(lu.variant)
          v_graph.vp["vertex_text"][n] = vertex_text
          # Ustawienie koloru etykiety
          # Etykieta jest kolorowana jezeli synset nalezy do kontekstu
          if v_graph.vp["context_synset"][n]:
            v_graph.vp["vertex_text_color"][n] = "red"
          else:
            v_graph.vp["vertex_text_color"][n] = "black"

          v_graph.vp["vertex_fill_color"][n] = [color_for_ranking_dict[v_graph.vp["PageRank"][n]], 0, 0, 1]

          # Sprawdzana jest tylko pierwsza
          # jednoskta leksykalna w synsecie i
          # z niej pobierane sa wszystkie informacje o nim
          break

      # Utworzenie PropertyMap, w ktorej przechowywane sa rozmiary wierzcholkow
      vertex_size_vp = prop_to_size(v_graph.vp["PageRank"], mi=5, ma=15)

      # Ustalenie pozycji wierzcholkow
      max_neighbours = 0
      chosen_vertex = 0
      for v in v_graph.vertices():
        number_of_neighbours = len([child for child in v.out_neighbours()])
        if max_neighbours < number_of_neighbours:
          max_neighbours = number_of_neighbours
          chosen_vertex = v

      pos_dict = defaultdict(set)

      queue = []
      discovered_list = []
      discovered_list.append(chosen_vertex)
      v_graph.vp["pos"][chosen_vertex] = [0.0, 0.0]
      for ngb in chosen_vertex.out_neighbours():
        queue.append(ngb)

      while queue:
        v = queue.pop(0)
        if not v in discovered_list:
          for parent in v.out_neighbours():
            if parent in discovered_list:
              break
          number_of_exist_parent_child = 0
          number_of_parent_childs = 0
          for child in parent.out_neighbours():
            number_of_parent_childs += 1
            if child in discovered_list:
              number_of_exist_parent_child += 1
          side_size = x = y = 0.0
          side_size = ((float(number_of_parent_childs) - 1.0) * 500.0 + float(number_of_parent_childs) * vertex_size_vp[v]) / 2.0 - vertex_size_vp[v] / 2.0
          x = (v_graph.vp["pos"][parent][0] - float(side_size)) + (float(number_of_exist_parent_child) - 1.0) * 500.0 + float(number_of_exist_parent_child) * vertex_size_vp[v]
          y = v_graph.vp["pos"][parent][1] - 1000.0
          if pos_dict.has_key(y):
            for x_cord in sorted(pos_dict[y]):
              while x_cord - 200.0 < x and x < x_cord + 200.0:
                x += 100
          pos_dict[y].add(x)
          v_graph.vp["pos"][v] = [x, y]
          discovered_list.append(v)
          for ngb in v.out_neighbours():
            queue.append(ngb)


      # Rysowanie grafu dla wszystkich czesci mowy
      graph_draw(v_graph, 
                 pos=v_graph.vp["pos"],
                 vertex_fill_color=v_graph.vp["vertex_fill_color"],
                 vertex_size=prop_to_size(v_graph.vp["PageRank"], mi=5, ma=15),
                 vertex_text=v_graph.vp["vertex_text"], 
                 vertex_text_color=v_graph.vp["vertex_text_color"],
                 vertex_font_size=4,
                 vertex_text_position=1,
                 vertex_text_rotation=3.14 / 2,
                 output=v_dir + 'all_POS_' + lemma + '(' + pos_str + ').pdf')

      # Rysowanie grafu dla rzeczownikow
      for n in v_graph.vertices():
        if n in nouns_nodes_not_to_filter_set:
          node_filter[n] = True
        else:
          node_filter[n] = False
      v_graph.set_vertex_filter(node_filter)
      graph_draw(v_graph, 
                 pos=v_graph.vp["pos"],
                 vertex_fill_color=v_graph.vp["vertex_fill_color"],
                 vertex_size=prop_to_size(v_graph.vp["PageRank"], mi=5, ma=15),
                 vertex_text=v_graph.vp["vertex_text"], 
                 vertex_text_color=v_graph.vp["vertex_text_color"],
                 vertex_font_size=4,
                 vertex_text_position=1,
                 vertex_text_rotation=3.14 / 2,
                 output=v_dir + 'nouns_' + lemma + '(' + pos_str + ').pdf')

      # Rysowanie grafu dla czasownikow
      v_graph.clear_filters()
      for n in v_graph.vertices():
        if n in verbs_nodes_not_to_filter_set:
          node_filter[n] = True
        else:
          node_filter[n] = False
      v_graph.set_vertex_filter(node_filter)
      graph_draw(v_graph, 
                 pos=v_graph.vp["pos"],
                 vertex_fill_color=v_graph.vp["vertex_fill_color"],
                 vertex_size=prop_to_size(v_graph.vp["PageRank"], mi=5, ma=15),
                 vertex_text=v_graph.vp["vertex_text"], 
                 vertex_text_color=v_graph.vp["vertex_text_color"],
                 vertex_font_size=4,
                 vertex_text_position=1,
                 vertex_text_rotation=3.14 / 2,
                 output=v_dir + 'verbs_' + lemma + '(' + pos_str + ').pdf')