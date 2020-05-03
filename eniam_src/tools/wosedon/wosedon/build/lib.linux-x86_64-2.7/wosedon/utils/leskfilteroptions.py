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

class LeskFilterOptions:
  def __init__(self):
    self._input_file = None
    self._allow_only = False
  
  def set_input_file(self, path):
    self._input_file = path
  
  def set_allow_only(self, val):
    self._allow_only = val
  
  def input_file(self):
    return self._input_file
  
  def allow_only(self):
    return self._allow_only
  
