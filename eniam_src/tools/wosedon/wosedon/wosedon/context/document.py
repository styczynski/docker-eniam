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

class Document(Context):
  def __init__(self, tagset, ccl_doc, ccl_rel = None):
    super(Document, self).__init__(tagset, ccl_doc, ccl_rel, 'Document')
    self.reset()
  
  def reset(self):
    self._read = False
  
  def next(self):
    if self._read:
      return None
    toks = []
    for p in self._ccl_doc.paragraphs():
      for s in p.sentences():
        toks.extend(s.tokens())
    self._read = True
    return WSDContext(tagset = self._tagset,
                      tokens = toks)
