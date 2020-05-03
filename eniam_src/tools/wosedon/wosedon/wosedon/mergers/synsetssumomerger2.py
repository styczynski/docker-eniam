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

class SynsetsSUMOMerger2(MergerInterface):
  """!
  Class for merging synsets graph and SUMO graph.

  SUMO graph was built based on the SUMO graph file, which is described in 
  SUMOGraphBuilder class. In SUMO graph each node represents one concept from ontology. 
  Whereas each edge represents relation occurring between them:
    - subclass,
    - subrelation,
    - instance,
    - subAttribute.

  The purpose of this class is to add edges from SUMO graph to synsets graph. 
  New edges between synsets are added according to relation between SUMO edges. 
  Thus, each edge from SUMO graph is inserted to the synsets graph.
  """
  def __init__(self, resources, options, str_name = 'SynsetsSUMOMerger2'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsSUMOMerger2"
    """
    super(SynsetsSUMOMerger2, self).__init__(resources, options, str_name)

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
    Merge two given graphs, namely synsets graph and SUMO graph. The final graph contain 
    one type of nodes, namely synsets nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas lexical units objects consists of: lu_id, 
    lemma, pos, variant and domain. The final graph contain two types of relation. 
    First type is relation occurring between synsets nodes only, which have assigned 
    relation identifier from PLWN. Second type is relation occurring between 
    SUMO concepts, but proper synsets nodes are connected. If two SUMO concepts are
    connected in SUMO graph and they have mapping on some synsets then synsets nodes 
    will be connected. This connection has two attributes: "rel" and "rel_type". 
    "rel" is a relation name from SUMO graph, whereas to "rel_type" is assigned word "sumo".

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    @param g1 - synsets graph
    @param g2 - SUMO graph
    @return object of BaseGraph class
    """
    logger = logging.getLogger(__name__)
    
    
    g = BaseGraph()
    g.copy_graph_from(g1)

    plwn2sumo_dict = defaultdict(set)
    plwn2sumo_dict = self.get_plwn2sumo_dict()

    synset_on_vertex_dict = {}
    for node in g.all_nodes():
      synset_id = node.synset.synset_id
      if synset_id in synset_on_vertex_dict:
        logger.warning("ID of some synset is not unique.")
        continue
      synset_on_vertex_dict[synset_id] = node

    num_of_edge = 0
    for edge in g2.all_edges():
      num_of_edge += 1
      logger.info("%d/%d", num_of_edge, g2.num_edges())

      parent_sumo_concept = edge.source().sumo
      child_sumo_concept = edge.target().sumo

      if parent_sumo_concept not in plwn2sumo_dict:
        logger.warning("The mapping file doesn't contain sumo concept '%s'.", parent_sumo_concept)
        continue
      if child_sumo_concept not in plwn2sumo_dict:
        logger.warning("The mapping file doesn't contain sumo concept '%s'.", child_sumo_concept)
        continue

      for parent_syn_id in plwn2sumo_dict[parent_sumo_concept]:
        if parent_syn_id not in synset_on_vertex_dict:
          logger.warning("The mapping file contains synset '%d' that is not in the graph.", parent_syn_id)
          continue
        p_node = synset_on_vertex_dict[parent_syn_id]
        for child_syn_id in plwn2sumo_dict[child_sumo_concept]:
          if child_syn_id not in synset_on_vertex_dict:
            logger.warning("The mapping file contains synset '%d' that is not in the graph.", child_syn_id)
            continue
          ch_node = synset_on_vertex_dict[child_syn_id]
          
          g.add_edge(p_node,
                     ch_node,
                     [("rel", edge.rel)],
                     simply=True)
    

    return g
