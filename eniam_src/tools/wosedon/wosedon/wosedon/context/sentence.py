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

from context import Context, WSDContext

class Sentence(Context):
  def __init__(self, tagset, ccl_doc, ccl_rel = None):
    super(Sentence, self).__init__(tagset, ccl_doc, ccl_rel, 'Sentence')
    self.reset()
    self._sentences = []
    for p in self._ccl_doc.paragraphs():
      self._sentences.extend(p.sentences())

  def reset(self):
    self._where_i_am = 0
  
  def next(self):
    if self._where_i_am >= len(self._sentences):
      return None
    s = self._sentences[self._where_i_am]
    self._where_i_am += 1
    return WSDContext(tagset = self._tagset,
                      tokens = s.tokens())