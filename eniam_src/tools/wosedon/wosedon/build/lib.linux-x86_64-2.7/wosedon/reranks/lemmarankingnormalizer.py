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
from wosedon.reranks.rerankinterface import RerankInterface

class LemmaRankingNormalizer(RerankInterface):
  """Normalizuje ranking dla lematu do przedzialu <0,1>.
  Ranking kazdego lematu (de facto tokenu) normalizoweny jest
  do przedzialu <0,1>"""
  
  def __init__(self, str_name = 'LemmaRankingNormalizer'):
    super(LemmaRankingNormalizer, self).__init__(str_name)

  def rerank(self, ranking, graph, options):
    new_rankg = []
    for (token, token_rank) in ranking:
      if not token_rank:
        new_rankg.append([token, token_rank])
        continue
      new_token_rank = []
      node_rank_sum = sum(float(n[1]) for n in token_rank)
      for (vertex, rank_val) in token_rank:
        new_token_rank.append((
          vertex, 
          float(rank_val) / node_rank_sum if node_rank_sum else 1.0))
      new_rankg.append((token, new_token_rank))
    return new_rankg
