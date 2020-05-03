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

class BuildOptions:
  def __init__(self):
    self._accept_lexicon = []
    self._unique_edges = True
    self._directed_graph = True
    self._syn_rel_ids = []
    self._lu_rel_ids = []
    self._accept_pos = []
    self._add_reversed_edges = {}

  def set_accept_lexicon(self, alexicon):
    self._accept_lexicon = alexicon

  def set_unique_edges(self, uqe):
    self._unique_edges = uqe

  def set_directed_graph(self, directed):
    self._directed_graph = directed
  
  def set_syn_rel_ids(self, sri):
    self._syn_rel_ids = sri
  
  def set_lu_rel_ids(self, lri):
    self._lu_rel_ids = lri
  
  def set_accept_pos(self, apos):
    self._accept_pos = apos

  def set_add_reversed_edges(self, are):
    self._add_reversed_edges = are

  def accept_lexicon(self):
    return self._accept_lexicon

  def has_unique_edges(self):
    return self._unique_edges

  def is_directed_graph(self):
    return self._directed_graph
  
  def syn_rel_ids(self):
    return self._syn_rel_ids
    
  def lu_rel_ids(self):
    return self._lu_rel_ids
  
  def accept_pos(self):
    return self._accept_pos
  
  def get_add_reversed_edges(self):
    return self._add_reversed_edges


