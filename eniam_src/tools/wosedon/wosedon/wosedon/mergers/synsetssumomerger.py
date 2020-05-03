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

import os, logging
import sys
from collections import defaultdict

from wosedon.basegraph import BaseGraph
from mergerinterface import MergerInterface

class SynsetsSUMOMerger(MergerInterface):
  """!
  Class for merging synsets graph and SUMO graph.

  SUMO graph was built based on the SUMO graph file, which is described in 
  SUMOGraphBuilder class. In SUMO graph each node represents one concept from ontology. 
  Whereas each edge represents relation occurring between them:
    - subclass,
    - subrelation,
    - instance,
    - subAttribute.

  The purpose of this class is to merge two graphs, namely synsets graph and 
  SUMO graph. Nodes from SUMO graph are conected to proper nodes from synsets graph, 
  based on mapping PLWN on SUMO ontology file.
  """
  def __init__(self, resources, options, str_name = 'SynsetsSUMOMerger'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsSUMOMerger"
    """
    super(SynsetsSUMOMerger, self).__init__(resources, options, str_name)

  def get_plwn2sumo_dict(self):
    """!
    Create dictionary based on mapping PLWN on SUMO ontology file. Dictionary format 
    and mapping PLWN on SUMO ontology file format are presented below.

    Format of mapping PLWN on SUMO ontology file:
    \code
    plwn_synid;plwn_domain;plwn_synsetrstr;plwn_pwn_interling_rel;pwn_synid;\
    pwn_domain;pwn_synsetstr;pwn_sumo_map_rel;sumo_concept;Serdel
    \endcode

    Sample format of mapping PLWN on SUMO ontology file:
    \code
    30;pos;{akcja-4(pos)};hipo_pa;356203;pos;\
    {capital-1(pos) working_capital-1(pos)};subsumed;MeanOfProduction;subsumed

    ...
    \endcode

    Format of dictionary:
    \code
    sumo_concept -> set([synset_id_1, ..., synset_id_N])
    \endcode

    Sample format of created dictionary:
    \code
    Lithium -> set([19247])
    WatchClock -> set([25849, 25851, 44443, 398925])
    ...
    \endcode

    @return dictionary, where key is a SUMO concept and value is a set of synsets 
            identifiers
    """
    if not os.path.exists(self.resources().mapping_sumo_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().mapping_sumo_file()
          )

    plwn2sumo_dict = defaultdict(set)

    with open(self.resources().mapping_sumo_file()) as sumofile:
      next(sumofile)
      for line in sumofile:
        synset_id = int(line.strip().split(';')[0])
        sumo = line.strip().split(';')[-2]
        plwn2sumo_dict[sumo].add(synset_id)
    
    return plwn2sumo_dict

  def merge(self, g1, g2):
    """!
    Merge two given graphs, namely synsets graph and SUMO graph. The final 
    graph contain two types of nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas lexical units objects consists of: lu_id, 
    lemma, pos, variant and domain. Each SUMO node has an attribute named "sumo", to which is 
    assigned a SUMO concept. To the relation between synsets nodes is assigned relation 
    identifier from PLWN. Whereas to the relation between SUMO nodes is assigned 
    relation name, which describe a type of connection between linked concepts. Based 
    on mapping PLWN on SUMO dictionary new relations are added between synsets nodes and 
    SUMO nodes.

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    Warning: If the final graph is a directed graph then two edges will be added between 
    a synset node and a SUMO node. First edge will be add from synset node to SUMO node 
    and second from SUMO node to synset node.

    @param g1 - synsets graph
    @param g2 - SUMO graph
    @return object of BaseGraph class
    """
    logger = logging.getLogger(__name__)
    
    g = BaseGraph()
    g.init_graph(drctd = g1.is_directed())

    plwn2sumo_dict = defaultdict(set)
    plwn2sumo_dict = self.get_plwn2sumo_dict()

    g.merge_graphs(g1, g2)

    nodes_dict = {}
    for node in g.all_nodes():
      synset_id = None
      if node.synset:
        synset_id = node.synset.synset_id
      sumo = None
      if node.sumo:
        sumo = node.sumo

      if synset_id and sumo:
        logger.warning("Synset and sumo are both in the same node.")
        continue
      if synset_id:
        if nodes_dict.has_key(synset_id):
          logger.warning("ID of some synset is not unique.")
          continue
        nodes_dict[synset_id] = node
      if sumo:
        if nodes_dict.has_key(sumo):
          logger.warning("Some sumo concept is not unique.")
          continue
        nodes_dict[sumo] = node

    for sumo, synset_id_set in plwn2sumo_dict.iteritems():
      if nodes_dict.has_key(sumo):
        sumo_node = nodes_dict[sumo]
        for synset_id in synset_id_set:
          if nodes_dict.has_key(synset_id):
            synset_id_node = nodes_dict[synset_id]
            g.add_edge(sumo_node, synset_id_node, [('rel', 'syn-sumo')], simply = True)
            if g.is_directed():
              g.add_edge(synset_id_node, sumo_node, [('rel', 'syn-sumo')], simply = True)
          else:
            logger.warning("The graph hasn't got synset %d, which is in the mapping file.", synset_id)
      else:
        logger.warning("The graph hasn't got sumo concept '%s', which is in the mapping file.", sumo)

    return g
