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

class SynsetsWNDMerger2(MergerInterface):
  """!
  Class for merging synsets graph and WND graph.

  WND graph was built based on the WND graph file, which is described in 
  WNDGraphBuilder class. In WND graph each node represents one domain/concept. 
  Whereas each edge represents relation occurring between them.

  The purpose of this class is to add edges from WND graph to synsets graph. 
  New edges between synsets are added according to relation between WND edges. 
  Thus, each edge from WND graph is inserted to the synsets graph.
  """
  def __init__(self, resources, options, str_name = 'SynsetsWNDMerger2'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsWNDMerger"
    """
    super(SynsetsWNDMerger2, self).__init__(resources, options, str_name)

  def get_plwn2wnd_dict(self):
    """!
    Create dictionary based on mapping PLWN on WND file. Dictionary format 
    and mapping PLWN on WND file format are presented below.

    Format of mapping PLWN on WND file:
    \code
    synset_str_plwn;domain_plwn;synset_id_plwn;interlingual_rel; \
    synset_str_pwn;pwn_synset_pos;domain_pwn;synset_id_pwn;wnd_domain
    \endcode

    Sample format of mapping PLWN on WND file:
    \code
    adres.5;por-7;16;syn_pa; \
    address.6;n;por-7;321457;geography

    adwokat.1;os-15;17;syncz_pa; \
    advocate.2;n;os-15;337414;law

    ...
    \endcode

    Format of dictionary:
    \code
    wnd_concept -> set([synset_id_1, ..., synset_id_N])
    \endcode

    Sample format of created dictionary:
    \code
    geography -> set([19247])
    law -> set([25849, 25851, 44443, 398925])
    ...
    \endcode

    @return dictionary, where key is a WND concept and value is a set of synsets 
            identifiers
    """
    if not os.path.exists(self.resources().mapping_wnd_file()):
      raise IOError(
        "%s file not found!" % \
          self.resources().mapping_wnd_file()
          )

    plwn2wnd_dict = defaultdict(set)

    with open(self.resources().mapping_wnd_file()) as wndfile:
      next(wndfile)
      for line in wndfile:
        synset_id = int(line.strip().split(';')[2])
        wnd = line.strip().split(';')[8]
        plwn2wnd_dict[wnd].add(synset_id)

    return plwn2wnd_dict

  def merge(self, g1, g2):
    """!
    Merge two given graphs, namely synsets graph and WND graph. The final graph contain 
    one type of nodes, namely synsets nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas lexical units objects consists of: lu_id, 
    lemma, pos, variant and domain. The final graph contain two types of relation. 
    First type is relation occurring between synsets nodes only, which have assigned 
    relation identifier from PLWN. Second type is relation occurring between 
    WND concepts, but proper synsets nodes are connected. If two WND concepts are
    connected in WND graph and they have mapping on some synsets then synsets nodes 
    will be connected. This connection has two attributes: "rel" and "rel_type". 
    "rel" is a relation name from WND graph, whereas to "rel_type" is assigned word "wnd".

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    @param g1 - synsets graph
    @param g2 - WND graph
    @return object of BaseGraph class
    """
    logger = logging.getLogger(__name__)
    
    
    g = BaseGraph()
    g.copy_graph_from(g1)

    plwn2wnd_dict = defaultdict(set)
    plwn2wnd_dict = self.get_plwn2wnd_dict()

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
      logger.info('%d/%d', num_of_edge, g2.num_edges())

      parent_wnd_concept = edge.source().wnd
      child_wnd_concept = edge.target().wnd

      if parent_wnd_concept not in plwn2wnd_dict:
        logger.warning("The mapping file doesn't contain wnd '%s'.", parent_wnd_concept)
        continue
      if child_wnd_concept not in plwn2wnd_dict:
        logger.warning("The mapping file doesn't contain wnd '%s'.", child_wnd_concept)
        continue

      for parent_syn_id in plwn2wnd_dict[parent_wnd_concept]:
        if parent_syn_id not in synset_on_vertex_dict:
          logger.warning("The mapping file contains synset '%d' that is not in the graph.", parent_syn_id)
          continue
        p_node = synset_on_vertex_dict[parent_syn_id]
        for child_syn_id in plwn2wnd_dict[child_wnd_concept]:
          if child_syn_id not in synset_on_vertex_dict:
            logger.warning("The mapping file contains synset '%d' that is not in the graph.", child_syn_id)
            continue
          ch_node = synset_on_vertex_dict[child_syn_id]
          g.add_edge(p_node,
                     ch_node,
                     [("rel", edge.rel)],
                     simply=True)

    return g
