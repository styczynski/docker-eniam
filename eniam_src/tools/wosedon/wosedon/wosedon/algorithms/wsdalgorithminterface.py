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

import abc

from collections import defaultdict

class WSDAlgorithmInterface(object):
  def __init__(self, str_name):
    self._str_name = str_name
    self._multiply_factor = 100000.0

  def __str__(self):
    return self._str_name

  @abc.abstractmethod
  def prepare_v(self, wsd_context, graph):
    raise NotImplementedError("WSDAlgorithmInterface!")

  @abc.abstractmethod
  def run(self, wsd_context, graph, options, resources = None):
    raise NotImplementedError("WSDAlgorithmInterface!")
