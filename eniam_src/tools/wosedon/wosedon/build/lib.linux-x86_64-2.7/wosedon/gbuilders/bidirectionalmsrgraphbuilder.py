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
from collections import defaultdict

from wosedon.basegraph import BaseGraph
from graphbuilderinterface import GraphBuilderInterface

class BidirectionalMSRGraphBuilder(GraphBuilderInterface):
  """!
  Class for building graph from k-best file.

  The built graph is based on the k-best file. K-best file contain words, where 
  for each word there are a list of k most relatedness word to it. To each word 
  in the list of k most relatedness words is also assigned the value of the 
  relatedness measure. K-best file is created based on the corpus and on 
  Measures of Semantic Relatedness.

  The difference between BidirectionalMSRGraphBuilder class and MSRGraphBuilder 
  class is that the BidirectionalMSRGraphBuilder class take into account only 
  symmetric (bidirectional) relations between words.

  Sample format of k-best file:
  \code
  subst:akcyza
    subst:podatek akcyzowy;0.491485
    subst:podatek;0.360689
    subst:agresja;0.235534
  subst:agresja
    subst:przemoc;0.334895
    subst:agresywnosc;0.327364
    subst:akcyza;0.235534
  ...
  \endcode
  """

  def __init__(self, resources, options, 
               str_name = 'BidirectionalMSRGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "BidirectionalMSRGraphBuilder"
    """
    super(BidirectionalMSRGraphBuilder, self).__init__(
      resources, options, str_name)
    self._kbests_dict = None

  def build_graph(self):
    """!
    Build graph based on _kbests_dict. Each node has an attribute named "msr" 
    which contain word and POS written as word/POS, e.g. akcyza/2. If word does not 
    have coarse POS, then will be ignored. To each edge is assigned weight, 
    which is the value of measure of semantic relatedness between linked words.

    The graph will be always undirected. Whereas all edges duplicates is removed.

    @return object of BaseGraph class
    """
    logger = logging.getLogger(__name__)
    
    
    if not os.path.exists(self.resources().msr_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().msr_file())

    self._read_kbests()
    self._bidirectional_kbest()

    g = BaseGraph()
    g.init_graph(drctd = False)

    g.create_node_attribute("msr", "string")
    g.create_edge_attributes([("weight", "double"), ('rel', 'string')])

    for parent, childs in self._kbests_dict.iteritems():
      lemma = parent.strip().split('/')[0]
      pl_pos = parent.strip().split('/')[1]
      pos_int = self._convert_to_coarse_pos(pl_pos)
      if not pos_int:
        logger.warning('Unknown POS "%s" for lemma "%s"', pl_pos, lemma)
        continue
      parent = lemma + '/' + str(pos_int)
      g.add_node(parent, [("msr", parent)])
      for (child, weight) in childs:
        lemma = child.strip().split('/')[0]
        pl_pos = child.strip().split('/')[1]
        pos_int = self._convert_to_coarse_pos(pl_pos)
        if not pos_int:
          logger.warning('Unknown POS "%s" for lemma "%s"', pl_pos, lemma)
          continue
        child = lemma + '/' + str(pos_int)
        g.add_node(child, [("msr", child)])
        if parent != child:
          g.add_edge(parent, child, [("weight", weight)])
  
    g.remove_edge_duplicates()

    return g

  def _read_kbests(self):
    """!
    Read k-best file and create a _kbest_dict dictionary in the format describe below. 
    The key in dictionary is a main word with part of speech (POS), where the 
    value is a set of tuples (word/POS, value of measure) from relatedness list.

    Sample format of created dictionary:
    \code
    akcyza/subst -> set([(podatek akcyzowy/subst, 0.491485), (podatek/subst, 0.360689), (agresja/subst, 0.235534)])
    agresja/subst -> set([(przemoc/subst, 0.334895), (agresywnosc/subst, 0.327364), (akcyza/subst, 0.235534)])
    ...
    \endcode
    """
    key = None
    self._kbests_dict = defaultdict(set)
    with open(self.resources().msr_file()) as msrgfile:
      for line in msrgfile:
        if not line.startswith('\t'):
          pl_pos = line.strip().split(':')[0]
          lemma = line.strip().split(':')[1]
          key = lemma + '/' + pl_pos
        else:
          pl_pos = line.strip().split(';')[0].split(':')[0]
          lemma = line.strip().split(';')[0].split(':')[1]
          weight = float(line.strip().split(';')[1])
          self._kbests_dict[key].add((lemma + '/' + pl_pos, weight))

  def _bidirectional_kbest(self):
    """!
    This function checks whether the word which is on the relatedness list of 
    the main word has also the main word on it's relatedness list. Thus, 
    whether the relation is symmetric or not. It should be noted, that parts of 
    speech are take into account during the checking process. If the word does not 
    have a main word on relatedness list it is removed from dictionary from main 
    word set. The content of dictionary will change from this given in _read_kbests 
    function to the:
    \code
    akcyza/subst -> set([(agresja/subst, 0.235534)])
    agresja/subst -> set([(akcyza/subst, 0.235534)])
    ...
    \endcode
    """
    # lista rodzicow do usuniecia, ktorzy nie maja dzieci
    parents_to_remove_list = []
    
    for parent, childs in self._kbests_dict.iteritems():
      # lista dzieci do usuniecia, 
      # ktore nie maja rodzicow jako kbest w swoim zbiorze
      childs_to_remove_list = []
      
      for (child, weight) in childs:
        if self._kbests_dict.has_key(child):
          if (parent, weight) in self._kbests_dict[child]:
            continue
          else:
            childs_to_remove_list.append((child, weight))
        else:
          childs_to_remove_list.append((child, weight))
      for (child, weight) in childs_to_remove_list:
        childs.remove((child, weight))
      if not childs:
        parents_to_remove_list.append(parent)
    for parent in parents_to_remove_list:
      del self._kbests_dict[parent]

  def _convert_to_coarse_pos(self, pl_pos):
    """!
    For a given fine POS return coarse POS in integer type.

    @param pl_pos - fine part of speech
    @return integer coarse part of speech where 1:verb, 2:noun, 3:adverb and 4:adjective
    """
    #! List of tags which can describe nouns.
    noun_pl_pos = ['subst', 'depr', 'ger', 'brev']
    #! List of tags which can describe adjectives.
    adj_pl_pos  = ['adj', 'adja', 'adjp', 'adjc']
    #! List of tags which can describe verbs.
    verb_pl_pos = ['fin', 'bedzie', 'praet', 'impt', \
                   'inf', 'pcon', 'pant', 'imps', \
                   'winien', 'pred', 'pact', 'ppas', 'pred']
    #! List of tags which can describe adverbs.
    adv_pl_pos  = ['adv']

    pos_int = None
    if pl_pos in noun_pl_pos:
      pos_int = 2
    elif pl_pos in adj_pl_pos:
      pos_int = 4
    elif pl_pos in verb_pl_pos:
      pos_int = 1
    elif pl_pos in adv_pl_pos:
      pos_int = 3
    return pos_int
