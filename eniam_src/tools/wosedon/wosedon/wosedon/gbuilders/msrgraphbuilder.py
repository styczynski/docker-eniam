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

import os
from wosedon.basegraph import BaseGraph
from graphbuilderinterface import GraphBuilderInterface

class MSRGraphBuilder(GraphBuilderInterface):
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
  def __init__(self, resources, options, str_name = 'MSRGraphBuilder'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of BuildOptions class contain build options from 
                       config file
    @param str_name  - default value same as the class name 
                       "MSRGraphBuilder"
    """
    super(MSRGraphBuilder, self).__init__(resources, options, str_name)

  def build_graph(self):
    """!
    Build graph based on k-best file. Each node has an attribute named "msr" 
    which contain word and POS written as word/POS, e.g. akcyza/2. If word does not 
    have coarse POS, then will be ignored. To each edge is assigned weight, 
    which is the value of measure of semantic relatedness between linked words.

    The graph will be always undirected.

    @return object of BaseGraph class
    """
    if not os.path.exists(self.resources().msr_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().msr_file())

    g = BaseGraph()
    g.init_graph(drctd = False)

    g.create_node_attribute("msr", "string")
    g.create_edge_attributes([("weight", "double"), ('rel', 'string')])

    parent = None
    child = None
    skip = False
    with open(self.resources().msr_file()) as msrgfile:
      for line in msrgfile:
        if not line.startswith('\t'):
          lemma = line.strip().split(':')[1]
          pos_int = self._convert_to_coarse_pos(line.strip().split(':')[0])
          if not pos_int:
            skip = True
          else:
            skip = False
            parent = lemma + '/' + str(pos_int)
            g.add_node(parent, [("msr", parent)])
        else:
          if skip:
            continue
          lemma = line.strip().split(':')[1].split(';')[0]
          pos_int = self._convert_to_coarse_pos(line.strip().split(':')[0])
          weight = float(line.strip().split(':')[1].split(';')[1])
          if not pos_int:
            continue
          else:
            child = lemma + '/' + str(pos_int)
            g.add_node(child, [("msr", child)])
            if parent != child:
              g.add_edge(parent, child, [("weight", weight)])
    
    if self.options().has_unique_edges():
      g.remove_edge_duplicates()

    return g

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
