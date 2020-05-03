# -*- coding: utf-8 -*-

import unittest
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.gbuilders.msrgraphbuilder import MSRGraphBuilder





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)




class TestMSRGB(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_msr_file('tests/#Data/msr_graph')
    self.opts = BuildOptions()
    self.opts.set_unique_edges(False)
  
  
  def test_edge_names(self):
    g = MSRGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertEquals(e.rel, '')
      self.assertIsInstance(e.weight, float)
  
  
  def test_numbers(self):
    g = MSRGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.all_nodes()), 7)
    self.assertEquals(leniter(g.all_edges()), 11)
  
  
  def test_unique(self):
    self.opts.set_unique_edges(True)
    g = MSRGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
    
    self.assertEquals(leniter(g.all_nodes()), 7)
    self.assertEquals(leniter(g.all_edges()), 8)



























