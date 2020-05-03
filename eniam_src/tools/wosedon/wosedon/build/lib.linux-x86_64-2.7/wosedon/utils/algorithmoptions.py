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

from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface 
from wosedon.utils.leskfilteroptions import LeskFilterOptions


MODULE_LESK_FUNCTIONS = 'wosedon.lesk_functions'
MODULE_LESK_FILTERS = 'wosedon.lesk_filters'



class AlgorithmOptions:
  def __init__(self):
    self._damping_factor = 0.85
    self._max_iter = 15
    self._ini_nodes = None
    
    # function to be used by Lesk
    self._lesk_function = None
    # filter to be used by Lesk
    self._lesk_filter = None
    # LeskFilterOptions object
    self._lesk_filter_opts = LeskFilterOptions()

  def set_damping_factor(self, df):
    self._damping_factor = df

  def set_max_iter(self, mi):
    self._max_iter = mi

  def set_ini_nodes(self, inodes):
    self._ini_nodes = inodes
    
  def set_lesk_function(self, func):
    self._lesk_function = func
    
  def set_lesk_filter(self, filtr):
    self._lesk_filter = filtr

  def damping_factor(self):
    return float(self._damping_factor)

  def max_iter(self):
    return int(self._max_iter)

  def ini_nodes(self):
    return self._ini_nodes

  def lesk_filter_opts(self):
    return self._lesk_filter_opts

  def lesk_function(self, resources, graph):
    if isinstance(self._lesk_function, str):
      module = __import__(MODULE_LESK_FUNCTIONS, fromlist=[self._lesk_function])
      self._lesk_function = getattr(module, self._lesk_function)(resources, self.lesk_filter(resources.tagset()), graph, self)
    return self._lesk_function

  def lesk_filter(self, tagset):
    if not isinstance(self._lesk_filter, LeskFilterInterface):
      if not self._lesk_filter:
        self._lesk_filter = 'Yes'
        
      module = __import__(MODULE_LESK_FILTERS, fromlist=[self._lesk_filter])
      self._lesk_filter = getattr(module, self._lesk_filter)(tagset, self._lesk_filter_opts)
    
    return self._lesk_filter









