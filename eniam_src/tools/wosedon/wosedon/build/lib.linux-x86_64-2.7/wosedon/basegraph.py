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

import itertools, logging

import graph_tool as gt
from graph_tool import Graph, load_graph
from graph_tool.generation import graph_union
from graph_tool.stats import remove_parallel_edges



class BaseNode(object):
  __slots__ = ['_graph', '_node']
  
  def __init__(self, graph, node):
    """
    @param graph:  graph to which the node belongs
    @type  graph:  graph_tool.Graph
    
    @param node:  underlying node
    @type  node:  graph_tool.Vertex
    """
    self._graph = graph
    self._node = node
  
  def use_graph_tool(self):
    """
    Returns underlying graph_tool.Vertex. It should be avoided at all costs. 
    """
    return self._node
  
  def all_edges(self):
    for e in self._node.all_edges():
      yield BaseEdge(self._graph, e)
  
  def all_neighbours(self):
    for n in self._node.all_neighbours():
      yield BaseNode(self._graph, n)
  
  def in_degree(self):
    return self._node.in_degree()
  
  def out_degree(self):
    return self._node.out_degree()
  
  def __getattr__(self, name):
    """
    allows acces to properties of graph
    """
    return self._graph.vp[name][self._node]
  
  
  def __setattr__(self, name, value):
    """
    allows acces to properties of graph
    """
    if name in self.__slots__:
      super(BaseNode, self).__setattr__(name, value)
    else:
      self._graph.vp[name][self._node] = value
  
  
  def __eq__(self, another):
    if isinstance(another, BaseNode):
      return self._node == another._node and self._graph == another._graph
    return self._node == another
    
  def __ne__(self, another):
    if isinstance(another, BaseNode):
      return self._node != another._node or self._graph != another._graph
    return self._node != another
  
  def __hash__(self):
    return hash(self._node)
  
  def __str__(self):
    return str(self._node)
  
  def __repr__(self):
    return repr(self._node)
  
  def __int__(self):
    return int(self._node)
  
  def copy(self):
    return BaseNode(self._graph, self._node)



class BaseEdge(object):
  __slots__ = ['_graph', '_edge']
  
  def __init__(self, graph, edge):
    """
    @param graph:  graph to which the edge belongs
    @type  graph:  graph_tool.Graph
    
    @param edge:  underlying edge
    @type  edge:  graph_tool.Edge
    """
    self._graph = graph
    self._edge = edge
  
  def use_graph_tool(self):
    """
    Returns underlying graph_tool.Edge. It should be avoided at all costs. 
    """
    return self._edge
  
  def __getattr__(self, name):
    """
    allows acces to properties of graph
    """
    return self._graph.ep[name][self._edge]
  
  
  def __setattr__(self, name, value):
    """
    allows acces to properties of graph
    """
    if name in self.__slots__:
      super(BaseEdge, self).__setattr__(name, value)
    else:
      self._graph.ep[name][self._edge] = value
  
  
  def __eq__(self, another):
    return self._edge == another._edge and self._graph == another._graph
    
  def __ne__(self, another):
    return self._edge != another._edge or self._graph != another._graph
  
  def __hash__(self):
    return hash(self._edge)
  
  def __str__(self):
    return str(self._edge)
  
  def __repr__(self):
    return repr(self._edge)
  
  def copy(self):
    return BaseEdge(self._graph, self._edge)
  
  
  def source(self):
    return BaseNode(self._graph, self._edge.source())
    
  def target(self):
    return BaseNode(self._graph, self._edge.target())





class BaseGraph(object):
  """
  Class representing a graph. We do not use pure graph_tool.Graph for we want
  to be able to easily change this library. Neither we use inheritance
  as graph_tool has inconvenient licence.
  """
  
  def __init__(self):
    self._g = None
    self._node_dict = {}
    self._syn_to_vertex_map = None
  
  def use_graph_tool(self):
    """
    Returns underlying graph_tool.Graph. It should be avoided at all costs. 
    """
    return self._g
  
  def get_node_for_synset_id(self, syn_id):
    """
    Lazy function to makes the map of synset identifiers to nodes into 
    the graph. The building of map is made only on the first funcion call.
    The first and the next calls of this function will return the built map.
    """
    if not self._syn_to_vertex_map:
      self._syn_to_vertex_map = {}
      for node in self.all_nodes():
        if node.synset:
          synset_id = node.synset.synset_id
          self._syn_to_vertex_map[synset_id] = node
    return self._syn_to_vertex_map.get(syn_id, None)

  def pickle(self, filename):
    self._g.save(filename)

  def unpickle(self, filename):
    self._g = load_graph(filename)

  def init_graph(self, drctd = False):
    self._g = Graph(directed = drctd)

  def copy_graph_from(self, g):
    self._g = g._g.copy()

  def set_directed(self, drctd):
    self._g.set_directed(drctd)

  def is_directed(self):
    return self._g.is_directed()

  def merge_graphs(self, g1, g2):
    self._g = graph_union(g1._g, g2._g, internal_props = True)

  # Node operations:
  def all_nodes(self):
    for node in self._g.vertices():
      yield BaseNode(self._g, node)

  def create_node_attribute(self, name, kind, value=None):
    if name not in self._g.vertex_properties:
      node_attr = self._g.new_vertex_property(kind, value)
      self._g.vertex_properties[name] = node_attr

  def create_node_attributes(self, node_attributes_list):
    for attr in node_attributes_list:
      if attr[0] not in self._g.vertex_properties:
        node_attr = self._g.new_vertex_property(attr[1])
        self._g.vertex_properties[attr[0]] = node_attr

  def add_node(self, name, node_attributes_list=[]):
    if name not in self._node_dict:
      new_node = self._g.add_vertex()
      self._node_dict[name] = BaseNode(self._g, new_node)
      for attr in node_attributes_list:
        self._g.vertex_properties[attr[0]][new_node] = attr[1]
    return self._node_dict[name]

  def get_node(self, name):
    return self._node_dict[name]
  
  def remove_node(self, name):
    self._g.remove_vertex(self._node_dict[name]._node)
    del self._node_dict[name]

  def nodes_filter(self, nodes_to_filter_set, inverted = False):
    node_filter = self._g.new_vertex_property("bool")
    for n in self.all_nodes():
      if n in nodes_to_filter_set:
        node_filter[n._node] = False
      else:
        node_filter[n._node] = True
    self._g.set_vertex_filter(node_filter, inverted)
    self._g.purge_vertices()

  # Edge operations:
  def num_edges(self):
    return self._g.num_edges()
  
  def all_edges(self):
    for e in self._g.edges():
      yield BaseEdge(self._g, e)
  
  def get_edges_between(self, source, target):
    """
    Return all edges between source and target. Source and target can be either
    BaseNode or integer.
    """
    if isinstance(source, BaseNode):
      source = source._node
    if isinstance(target, BaseNode):
      target = target._node
    for e in self._g.edge(source, target, all_edges=True):
      yield BaseEdge(self._g, e)
    
  def get_edge(self, source, target, add_missing=False):
    """
    Return some edge between source and target. Source and target can be either
    BaseNode or integer.
    """
    if isinstance(source, BaseNode):
      source = source._node
    if isinstance(target, BaseNode):
      target = target._node
    e = self._g.edge(source, target, add_missing)
    if e is not None:
      return BaseEdge(self._g, e)
    else:
      return None

  def create_edge_attribute(self, name, kind, value=None):
    if name not in self._g.edge_properties:
      edge_attr = self._g.new_edge_property(kind, value)
      self._g.edge_properties[name] = edge_attr

  def alias_edge_attribute(self, name, alias): 
    self._g.edge_properties[alias] = self._g.edge_properties[name]

  def create_edge_attributes(self, edge_attributes_list): 
    for attr in edge_attributes_list:
      if attr[0] not in self._g.edge_properties:
        edge_attr = self._g.new_edge_property(attr[1])
        self._g.edge_properties[attr[0]] = edge_attr

  def add_edge(self, parent, child, edge_attributes_list = [], simply = False):
    if simply:
      new_edge = self._g.add_edge(parent._node, child._node)
    else:
      if self._node_dict.has_key(parent) \
      and self._node_dict.has_key(child):
        n_parent = self._node_dict[parent]._node
        n_child = self._node_dict[child]._node
        new_edge = self._g.add_edge(n_parent, n_child)
    for attr in edge_attributes_list:
      self._g.edge_properties[attr[0]][new_edge] = attr[1]
    return BaseEdge(self._g, new_edge)

  def edges_filter(self, edges_to_filter_set):
    edge_filter = self._g.new_edge_property("bool")
    for e in self.all_edges():
      if e in edges_to_filter_set:
        edge_filter[e._edge] = False
      else:
        edge_filter[e._edge] = True
    self._g.set_edge_filter(edge_filter)
    self._g.purge_edges()

  def remove_edge_duplicates(self):
    # WARNING 
    # Uwaga! Usuwa wszystkie krawedzie. Nie patrzy na typ krawedzi
    # ani na wage krawedzi, usuwa wszystko i zostawia tylko jedna
    # krawedz. Aby zostawic okreslone krawedzie nalezy repimplementowac
    # ta metode.
    remove_parallel_edges(self._g)
  
  def ungraph_tool(self, thingy):
    """
    Converts given data structure so that it no longer have any graph_tool dependencies.
    """
    logger = logging.getLogger(__name__)
    
    if type(thingy) == dict:
      return {self.ungraph_tool(k) : self.ungraph_tool(thingy[k]) for k in thingy}
    
    if type(thingy) == gt.PropertyMap:
      dct = {}
      if thingy.key_type() == 'v':
        for node in self.all_nodes():
          dct[node] = thingy[node.use_graph_tool()]
      elif thingy.key_type() == 'e':
        for edge in self.all_edges():
          dct[edge] = thingy[edge.use_graph_tool()]
      else:
        logger.error('Unknown property type %s', thingy.key_type())
        raise NotImplemented
      return dct
    
    if type(thingy) == set:
      return {self.ungraph_tool(k) for k in thingy}
    
    if type(thingy) == list:
      return [self.ungraph_tool(k) for k in thingy]
    
    if type(thingy) == gt.Node:
      return BaseNode(self, thingy)
    
    if type(thingy) == gt.Edge:
      return BaseEdge(self, thingy)
    
    return thingy
















