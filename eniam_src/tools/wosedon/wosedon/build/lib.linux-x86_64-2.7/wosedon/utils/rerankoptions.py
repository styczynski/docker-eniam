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

import corpus2

class RerankOptions:
  def __init__(self):
    # corpus2 tagset object
    self._tagset = None
    self._percentage_diff = 0

  def set_tagset(self, tagset):
    self._tagset = tagset

  def set_percentage_diff(self, pd):
    self._percentage_diff = pd

  def tagset(self):
    return self._tagset

  def percentage_diff(self):
    return int(self._percentage_diff)