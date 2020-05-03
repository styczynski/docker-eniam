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

import os, sys, logging
import corpus2
from collections import defaultdict

class Resources:
  def __init__(self):
    # corpus2 tagset object
    self._tagset = None

    self._msr_file = None
    
    self._plwn_graph_file = None

    self._wnd_graph_file = None
    self._sumo_graph_file = None
    self._ccl_graph_file = None

    self._mapping_sumo_file = None
    self._mapping_wnd_file = None

    self._gloss_file = None
    self._gloss_rel_file = None
    
    self._impedance_table = None

  def tagset(self):
    if not isinstance(self._tagset, corpus2.Tagset):
      self._tagset = corpus2.get_named_tagset(self._tagset)
    return self._tagset

  def msr_file(self):
    return self._msr_file
  
  def plwn_graph_file(self):
    return self._plwn_graph_file

  def wnd_graph_file(self):
    return self._wnd_graph_file

  def sumo_graph_file(self):
    return self._sumo_graph_file

  def ccl_graph_file(self):
    return self._ccl_graph_file

  def mapping_wnd_file(self):
    return self._mapping_wnd_file

  def mapping_sumo_file(self):
    return self._mapping_sumo_file

  def gloss_file(self):
    if isinstance(self._gloss_file, str):
      self._prepare_gloss_dict()
    return self._gloss_file

  def gloss_rel_file(self):
    return self._gloss_rel_file
  
  def impedance_table(self):
    if not isinstance(self._impedance_table, defaultdict):
      it = defaultdict(lambda: defaultdict(float))
      with open(self._impedance_table) as f:
        headers = f.readline().strip().split(',')
        headers = [h.strip() for h in headers]
        
        for l in f:
          l = l.strip().split(',')
          row = l[0].strip()
          it[row] = defaultdict(float)
          for i in xrange(1, len(headers)):
            it[row][headers[i]] = float(l[i])
      
      self._impedance_table = it
    return self._impedance_table

  def set_tagset(self, tagset):
    self._tagset = tagset

  def set_msr_file(self, msr_file):
    self._msr_file = msr_file

  def set_plwn_graph_file(self, pgf):
    self._plwn_graph_file = pgf
  
  def set_wnd_graph_file(self, wgf):
    self._wnd_graph_file = wgf
  
  def set_sumo_graph_file(self, sgf):
    self._sumo_graph_file = sgf
  
  def set_ccl_graph_file(self, cgf):
    self._ccl_graph_file = cgf

  def set_mapping_sumo_file(self, msf):
    self._mapping_sumo_file = msf

  def set_mapping_wnd_file(self, mwf):
    self._mapping_wnd_file = mwf

  def set_gloss_file(self, gf):
    self._gloss_file = gf

  def set_gloss_rel_file(self, grf):
    self._gloss_rel_file = grf
  
  def set_impedance_table(self, path):
    self._impedance_table = path

  def _prepare_gloss_dict(self):
    if not os.path.exists(self._gloss_file):
      raise IOError(
        "%s file not found!" % \
          self._gloss_file)

    try:
      cclreader = corpus2.CclRelReader(
                      self.tagset(), 
                      self._gloss_file, 
                      self._gloss_file
                  )
      document = cclreader.read()
      self._gloss_file = {}
      for paragraph in document.paragraphs():
        synset_id = int(paragraph.get_attribute('id'))
        self._gloss_file[synset_id] = paragraph
    except Exception, e:
      logging.getLogger(__name__).exception('Cannot load gloss file.')
      raise
