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

class SynsetsWNDMerger(MergerInterface):
  """!
  Class for merging synsets graph and WND graph.

  WND graph was built based on the WND graph file, which is described in 
  WNDGraphBuilder class. In WND graph each node represents one domain/concept. 
  Whereas each edge represents relation occurring between them.

  The purpose of this class is to merge two graphs, namely synsets graph and 
  WND graph. Nodes from WND graph are conected to proper nodes from synsets graph, 
  based on mapping PLWN on WND file.
  """
  def __init__(self, resources, options, str_name = 'SynsetsWNDMerger'):
    """!
    @param resources - object of Resources class contain resources from config 
                       file
    @param options   - object of MergeOptions class contain merge options from 
                       config file
    @param str_name  - default value same as the class name "SynsetsWNDMerger"
    """
    super(SynsetsWNDMerger, self).__init__(resources, options, str_name)

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
    Merge two given graphs, namely synsets graph and WND graph. The final 
    graph contain two types of nodes. Each synset node has an attribute named "synset", 
    to which is assigned synset object. Synset object consist of: synset_id and lu_set, 
    which contains lexical units objects. Whereas lexical units objects consists of: lu_id, 
    lemma, pos, variant and domain. Each WND node has an attribute named "wnd", to which is 
    assigned a WND concept. To the relation between synsets nodes is assigned relation 
    identifier from PLWN. Whereas to the relation between WND nodes is assigned 
    relation name, which describe a type of connection between linked concepts. Based 
    on mapping PLWN on WND dictionary new relations are added between synsets nodes and 
    WND nodes.

    The directness of the graph and uniqueness of edges are inherited from synsets graph.

    Warning: If the final graph is a directed graph then two edges will be added between 
    a synset node and a WND node. First edge will be add from synset node to WND node 
    and second from WND node to synset node.

    @param g1 - synsets graph
    @param g2 - WND graph
    @return object of BaseGraph class
    """
    logger = logging.getLogger(__name__)
    
    
    g = BaseGraph()
    g.init_graph(drctd = g1.is_directed())

    plwn2wnd_dict = defaultdict(set)
    plwn2wnd_dict = self.get_plwn2wnd_dict()

    g.merge_graphs(g1, g2)

    nodes_dict = {}
    for node in g.all_nodes():
      synset_id = None
      if node.synset:
        synset_id = node.synset.synset_id
      wnd = None
      if node.wnd:
        wnd = node.wnd

      if synset_id and wnd:
        logger.warning("Synset and wnd are both in the same node.")
        continue
      if synset_id:
        if nodes_dict.has_key(synset_id):
          logger.warning("ID of some synset is not unique.")
          continue
        nodes_dict[synset_id] = node
      if wnd:
        if nodes_dict.has_key(wnd):
          logger.warning("WND is not unique.")
          continue
        nodes_dict[wnd] = node

    for wnd, synset_id_set in plwn2wnd_dict.iteritems():
      if nodes_dict.has_key(wnd):
        wnd_node = nodes_dict[wnd]
        for synset_id in synset_id_set:
          if nodes_dict.has_key(synset_id):
            synset_id_node = nodes_dict[synset_id]
            g.add_edge(wnd_node, synset_id_node, [("rel", 'syn-wnd')], simply = True)
            if g.is_directed():
              g.add_edge(synset_id_node, wnd_node, [("rel", 'syn-wnd')], simply = True)
          else:
            logger.warning("The graph hasn't got synset %d, which is in the mapping file.", synset_id)
      else:
        logger.warning("The graph hasn't got wnd '%s', which is in the mapping file.", wnd)

    return g
