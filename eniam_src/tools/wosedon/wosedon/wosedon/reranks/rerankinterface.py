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

import abc

class RerankInterface(object):
  def __init__(self, str_name):
    self._str_name = str_name

  def __str__(self):
    """String-like representation of reranker"""
    return self._str_name

  @abc.abstractmethod
  def rerank(self, ranking, graph, options):
    """Abstract method. Should be implemented by all rerankers.

    @param ranking - ranking returned from wsdakgorithm.run() method
    @param graph - built graph, object of BaseGraph
    @returns new, reranked ranking with format as below:
    
    [
      (
        token, 
        [
          (<Vertex object with index '6085' at 0x7f91e0a50350>, ranking_value),
          ...
        ]
      ),
      ...
    ]
    """
    raise NotImplementedError("RerankInterface!")
