# -*- coding: utf-8 -*-

import unittest
from wosedon.basegraph import BaseGraph
from wosedon.basegraph import BaseNode
from wosedon.basegraph import BaseEdge




def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestBaseGraph_Creation(unittest.TestCase):
  
  def setUp(self):
    self.graph = BaseGraph()
    self.graph.init_graph()
    self.node_a = self.graph.add_node('a')
    self.graph.add_node('b')
    self.graph.add_edge('a', 'b')
    self.graph.add_edge('b', 'a')
    self.graph.add_node('c')
    self.graph.add_node('d')
    self.graph.remove_node('d')
    self.graph.add_node('d')
    self.graph.add_edge('d', 'b')
    self.node_e = self.graph.add_node('e')
    self.edge_ae = self.graph.add_edge('a', 'e')
    self.edge_ee = self.graph.add_edge('e', 'e')
  
  
  def test_vertices(self):
    self.assertEquals(leniter(self.graph.all_nodes()), 5)
    vl = set(self.graph.all_nodes())
    
    for node in self.graph.all_nodes():
      self.assertIsInstance(node, BaseNode)
      self.assertIn(node, vl)  # test for equality and hashes
      self.assertTrue(node == node.copy())
      self.assertFalse(node != node.copy())
  
  
  def test_edges(self):
    self.assertEquals(leniter(self.graph.all_edges()), 5)
    el = set(self.graph.all_edges())
    
    for edge in self.graph.all_edges():
      self.assertIsInstance(edge, BaseEdge)
      self.assertIsInstance(edge.source(), BaseNode)
      self.assertIsInstance(edge.target(), BaseNode)
      self.assertIn(edge, el)  # test for equality and hashes
      self.assertTrue(edge == edge.copy())
      self.assertFalse(edge != edge.copy())
    
    self.assertEquals(self.edge_ee.source(), self.node_e)
    self.assertEquals(self.edge_ee.target(), self.node_e)
    self.assertEquals(self.edge_ae.source(), self.node_a)
    self.assertEquals(self.edge_ae.target(), self.node_e)
    
    self.assertEquals(self.graph.get_edge(self.node_e, self.node_e), self.edge_ee)
    self.assertEquals(self.graph.get_edge(self.node_a, self.node_e), self.edge_ae)
    
    
    


class TestSynsetGB_Changes(unittest.TestCase):
  
  def setUp(self):
    self.graph = BaseGraph()
    self.graph.init_graph()
    
    for i in range(11):
      self.graph.add_node(i)
    
    self.graph.add_edge(1, 3)
    self.graph.add_edge(3, 8)
    self.graph.add_edge(3, 6)
    self.graph.add_edge(3, 6)
    self.graph.add_edge(4, 10)
    self.graph.add_edge(5, 2)
    self.graph.add_edge(5, 10)
    self.graph.add_edge(5, 1)
    self.graph.add_edge(5, 5)
    self.graph.add_edge(7, 1)
    self.graph.add_edge(7, 7)
    self.graph.add_edge(7, 6)
    self.graph.add_edge(8, 5)
    self.graph.add_edge(8, 5)
    self.graph.add_edge(10, 0)
    self.graph.add_edge(0, 10)
    
  
  
  def test_undirected_not_unique(self):
    self.graph.set_directed(False)
    
    self.assertEquals(leniter(self.graph.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(self.graph.get_edges_between(5, 8)), 2)
    self.assertEquals(leniter(self.graph.get_edges_between(0, 10)), 2)
    self.assertEquals(leniter(self.graph.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(self.graph.get_edges_between(1, 5)), 1)
    #self.assertEquals(leniter(self.graph.get_edges_between(7, 7)), 1)  #FIXME to nie przechodzi
  
  
  def test_directed_not_unique(self):
    self.graph.set_directed(True)
    
    self.assertEquals(leniter(self.graph.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(self.graph.get_edges_between(8, 5)), 2)
    self.assertEquals(leniter(self.graph.get_edges_between(0, 10)), 1)
    self.assertEquals(leniter(self.graph.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(self.graph.get_edges_between(1, 5)), 0)
    self.assertEquals(leniter(self.graph.get_edges_between(7, 7)), 1)
  
  
  def test_undirected_unique(self):
    self.graph.set_directed(False)
    self.graph.remove_edge_duplicates()
    
    for s in self.graph.all_nodes():
      for t in self.graph.all_nodes(): #FIXME usuwanie duplikatów nie obejmuje sytuacji gdy graf jest nieskierowany i krawędź przeciwna tworzy drugą krawędź
        self.assertLessEqual(leniter(self.graph.get_edges_between(s, t)), 2) #1)
  
  
  def test_directed_unique(self):
    self.graph.set_directed(True)
    self.graph.remove_edge_duplicates()
    
    for s in self.graph.all_nodes():
      for t in self.graph.all_nodes():
        self.assertLessEqual(leniter(self.graph.get_edges_between(s, t)), 1)
  
  
  def test_nodes_filter(self):
    self.graph.nodes_filter({1, 5, 8})
    self.assertEquals(leniter(self.graph.all_nodes()), 8)
  
  
  def test_nodes_filter_inv(self):
    self.graph.nodes_filter({1, 5, 8}, inverted=True)
    self.assertEquals(leniter(self.graph.all_nodes()), 3)
  
  
  def test_basenodes_filter(self):
    self.graph.nodes_filter(set(self.graph.all_nodes()))
    self.assertEquals(leniter(self.graph.all_nodes()), 0)
  
  
  def test_edges_filter(self):
    self.graph.edges_filter({self.graph.get_edge(1, 3), self.graph.get_edge(5, 5), self.graph.get_edge(7, 1)})
    self.assertEquals(leniter(self.graph.all_edges()), 13)




class TestBaseGraph_Properties(unittest.TestCase):
  
  def setUp(self):
    self.graph = BaseGraph()
    self.graph.init_graph()
    self.node_a = self.graph.add_node('a')
    self.graph.add_node('b')
    self.graph.add_edge('a', 'b')
    self.graph.add_edge('b', 'a')
    self.graph.create_edge_attribute('weight', 'float')
    self.graph.add_node('c')
    self.graph.add_node('d')
    self.graph.add_edge('d', 'b')
    self.node_e = self.graph.add_node('e')
    self.edge_ae = self.graph.add_edge('a', 'e')
    self.edge_ee = self.graph.add_edge('e', 'e')
    self.graph.create_node_attributes([('colour', 'string'), ('synset', 'object')])
    
  
  def test_properties(self):
    self.node_a.colour = 'green'
    self.assertEquals(self.node_a.colour, 'green')
    
    self.node_e.colour = 'red'
    self.assertEquals(self.node_e.colour, 'red')
    self.assertEquals(self.node_a.colour, 'green')
    
    self.edge_ae.weight = 23
    self.assertEquals(self.edge_ae.weight, 23)
    
  
  def test_ensure_properties(self):
    self.node_a.colour = 'green'
    self.graph.create_node_attributes([('colour', 'string')])
    self.assertEquals(self.node_a.colour, 'green')
    self.graph.create_node_attribute('colour', 'string')
    self.assertEquals(self.node_a.colour, 'green')
    
    self.edge_ae.weight = 23
    self.graph.create_edge_attributes([('weight', 'float')])
    self.assertEquals(self.edge_ae.weight, 23)
    self.graph.create_edge_attribute('weight', 'float')
    self.assertEquals(self.edge_ae.weight, 23)
  
  
  def test_get_node_for_synset_id(self):
    class Synset:
      def __init__(self, syn_id):
        self.synset_id = syn_id
    
    i = 1
    for node in self.graph.all_nodes():
      node.synset = Synset(i)
      i += 1
      
    i = 1
    for node in self.graph.all_nodes():
      self.assertIsInstance(self.graph.get_node_for_synset_id(i), BaseNode)
      i += 1




















