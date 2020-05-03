#
# <one line to give the library's name and an idea of what it does.>
# Copyright (C) 2015  <copyright holder> <email>
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

import operator
import rerankinterface

class NodeDegreeRanker(rerankinterface.RerankInterface):
  """Kazdy element rankingu (synset/wezel grafu) 
  wazony jest sopniem tego wezla."""

  def __init__(self, str_name = 'NodeDegreeRanker'):
    super(NodeDegreeRanker, self).__init__(str_name)

  def rerank(self, ranking, graph, options):
    new_rankg = []
    for (token, token_rank) in ranking:
      if not token_rank:
        new_rankg.append([token, token_rank])
        continue
      new_token_rank = []
      for (vertex, rank_val) in token_rank:
        deg = vertex.in_degree() + vertex.out_degree() if vertex else 1
        new_token_rank.append((vertex, rank_val * deg))
      new_rankg.append(
        (token, 
         sorted(
           new_token_rank, 
           key = operator.itemgetter(1), 
           reverse = True)))
    return new_rankg
