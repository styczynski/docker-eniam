# -*- coding: utf-8 -*-

import unittest
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.gbuilders.bidirectionalmsrgraphbuilder import BidirectionalMSRGraphBuilder





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestBidirectionalMSRGB_Internals(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_msr_file('tests/#Data/msr_graph')
    opts = BuildOptions()
    self.gb = BidirectionalMSRGraphBuilder(res, opts)
  
  def test_read_kbests(self):
    self.gb._read_kbests()
    
    self.assertEquals(len(self.gb._kbests_dict), 5)
    
    cnt = 0
    for k in self.gb._kbests_dict:
      for a, b in self.gb._kbests_dict[k]:
        self.assertIsInstance(a, str)
        self.assertIsInstance(b, float)
        cnt += 1
    
    self.assertEquals(cnt, 11)
  
  def test_bidirectional_kbest(self):
    self.gb._read_kbests()
    self.gb._bidirectional_kbest()
    
    self.assertEquals(len(self.gb._kbests_dict), 4)
    
    cnt = 0
    for k in self.gb._kbests_dict:
      for a, b in self.gb._kbests_dict[k]:
        self.assertIsInstance(a, str)
        self.assertIsInstance(b, float)
        cnt += 1
    
    self.assertEquals(cnt, 4)





class TestBidirectionalMSRGB(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_msr_file('tests/#Data/msr_graph')
    self.opts = BuildOptions()
  
  
  def test_edge_names(self):
    g = BidirectionalMSRGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertEquals(e.rel, '')
      self.assertIsInstance(e.weight, float)
  
  
  def test_numbers(self):
    g = BidirectionalMSRGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.all_nodes()), 4)
    self.assertEquals(leniter(g.all_edges()), 2)
  
  
  def test_unique(self):
    g = BidirectionalMSRGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)



























