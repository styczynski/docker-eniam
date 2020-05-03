# -*- coding: utf-8 -*-

import unittest
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.gbuilders.wndgraphbuilder import WNDGraphBuilder





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestWNDGB_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_wnd_graph_file('tests/#Data/wnd_graph')
    self.opts = BuildOptions()
  
  
  def test_numbers(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
    
    g = WNDGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.all_nodes()), 10)
    self.assertEquals(leniter(g.all_edges()), 16)
  
  
  def test_undirected_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(True)
    
    g = WNDGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        if s != t:
          self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
  
  
  def test_directed_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(True)
    
    g = WNDGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
        



class TestWNDGB(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_wnd_graph_file('tests/#Data/wnd_graph')
    self.opts = BuildOptions()
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
  
  
  def test_edge_names(self):
    g = WNDGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertEquals(e.rel, 'isa')



























