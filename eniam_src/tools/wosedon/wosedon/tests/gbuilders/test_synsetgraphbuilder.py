# -*- coding: utf-8 -*-

import unittest
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.gbuilders.synsetgraphbuilder import SynsetGraphBuilder



def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)



class TestSynsetGB_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_plwn_graph_file('tests/#Data/plwnik')
    self.opts = BuildOptions()
  
  
  def test_undirected_not_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(False)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(g.get_edges_between(5, 8)), 2)
    self.assertEquals(leniter(g.get_edges_between(0, 10)), 2)
    self.assertEquals(leniter(g.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(g.get_edges_between(1, 5)), 1)
  
  
  def test_directed_not_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(3, 6)), 2)
    self.assertEquals(leniter(g.get_edges_between(8, 5)), 2)
    self.assertEquals(leniter(g.get_edges_between(0, 10)), 1)
    self.assertEquals(leniter(g.get_edges_between(5, 1)), 1)
    self.assertEquals(leniter(g.get_edges_between(1, 5)), 0)
    self.assertEquals(leniter(g.get_edges_between(7, 7)), 1)
  
  
  def test_undirected_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(True)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        if s != t:
          self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
  
  
  def test_directed_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(True)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)




class TestSynsetGB_DirectnessAndDuplicatesWithReverseEdges(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_plwn_graph_file('tests/#Data/plwnik')
    self.opts = BuildOptions()
    self.opts.set_add_reversed_edges({11:1011})
  
  
  def test_undirected_not_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(False)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(1, 3)), 2)
    self.assertEquals(leniter(g.get_edges_between(5, 8)), 3)
  
  
  def test_directed_not_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    self.assertEquals(leniter(g.get_edges_between(3, 1)), 1)
    self.assertEquals(leniter(g.get_edges_between(5, 8)), 1)
  
  
  def test_undirected_unique(self):
    self.opts.set_directed_graph(False)
    self.opts.set_unique_edges(True)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        if s != t:
          self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
  
  
  def test_directed_unique(self):
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(True)
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for s in g.all_nodes():
      for t in g.all_nodes():
        self.assertLessEqual(leniter(g.get_edges_between(s, t)), 1)
        



class TestSynsetGB(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
    self.res.set_plwn_graph_file('tests/#Data/plwnik')
    self.opts = BuildOptions()
    self.opts.set_directed_graph(True)
    self.opts.set_unique_edges(False)
    self.opts.set_add_reversed_edges({11:1011, 13:13, 12:14})
  
  
  def test_syn_rel_ids(self):
    self.opts.set_syn_rel_ids(set([11, 15]))
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertIn(e.rel_id, [11, 15])
  
  
  #TODO test_accept_lexicon
  
  
  def test_accept_pos(self):
    self.opts.set_accept_pos(set([1,3]))
    
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for n in g.all_nodes():
      for lu in n.synset.lu_set:
        self.assertIn(lu.pos, [1,3])
  
  
  def test_add_reversed_edges(self):
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      if e.rel_id == 11:
        self.assertIn(1011, [k.rel_id for k in g.get_edges_between(e.target(), e.source())])
      elif e.rel_id == 13:
        self.assertIn(13, [k.rel_id for k in g.get_edges_between(e.target(), e.source())])
      elif e.rel_id == 12:
        self.assertIn(14, [k.rel_id for k in g.get_edges_between(e.target(), e.source())])
  
  
  def test_edge_names(self):
    g = SynsetGraphBuilder(self.res, self.opts).build_graph()
    
    for e in g.all_edges():
      self.assertRegexpMatches(e.rel, r's\d+')



























