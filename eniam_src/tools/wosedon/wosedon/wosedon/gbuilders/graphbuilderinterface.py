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

class GraphBuilderInterface(object):
  def __init__(self, resources, options, str_name):
    self._resources = resources
    self._build_options = options
    self._str_name = str_name

  def __str__(self):
    return self._str_name

  def resources(self):
    return self._resources

  def options(self):
    return self._build_options

  @abc.abstractmethod
  def build_graph(self):
    raise NotImplementedError("GraphBuilderInterface!")
