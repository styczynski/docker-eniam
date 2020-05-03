# -*- coding: utf-8 -*-

import unittest
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.gbuilders.cclgraphbuilder import CCLGraphBuilder





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestCCLGB_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_ccl_graph_file('tests/#Data/cclgraph.xml')
    self.opts = BuildOptions()
  
  
  def test_undirected_not_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(False)
    
    g = CCLGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(g.get_edges_between(5, 8)), 2)
    self.assertEquals(leniter(g.get_edges_between(0, 10)), 2)
    self.assertEquals(leniter(g.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(g.get_edges_between(1, 5)), 1)
  
  
  def test_directed_not_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
    
    g = CCLGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(g.get_edges_between(8, 5)), 2)
    self.assertEquals(leniter(g.get_edges_between(0, 10)), 1)
    self.assertEquals(leniter(g.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(g.get_edges_between(1, 5)), 0)
    self.assertEquals(leniter(g.get_edges_between(7, 7)), 1)
  
  
  def test_undirected_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(True)
    
    g = CCLGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        if s != t:
          self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
  
  
  def test_directed_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(True)
    
    g = CCLGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
        



class TestCCLGB(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_ccl_graph_file('tests/#Data/cclgraph.xml')
    self.opts = BuildOptions()
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
  
  
  def test_edge_names(self):
    g = CCLGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertIn(e.rel, ['in_sentence', 'in_paragraph', 'in_document'])



























