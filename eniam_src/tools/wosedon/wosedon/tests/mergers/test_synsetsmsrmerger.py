# -*- coding: utf-8 -*-

import unittest, re
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.utils.mergeoptions import MergeOptions
from wosedon.gbuilders import SynsetGraphBuilder
from wosedon.gbuilders import MSRGraphBuilder
from wosedon.mergers import SynsetsMSRMerger





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)



class TestSynsetsMSRMerger_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    res.set_msr_file('tests/#Data/msr_graph')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(False)
    bopts.set_unique_edges(True)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gm = MSRGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsMSRMerger(res, mopts).merge(gs, gm)
  
  
  def test_undirected_unique(self):
    for s in self.graph.all_nodes():
      for t in self.graph.all_nodes():
        if s != t:
          if leniter(self.graph.get_edges_between(s, t)) > 1:
            self.fail((s, t))
          self.assertLessEqual(leniter(self.graph.get_edges_between(s, t)), 1)
        



class TestSynsetsMSRMerger(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    res.set_msr_file('tests/#Data/msr_graph')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(True)
    bopts.set_unique_edges(False)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gm = MSRGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsMSRMerger(res, mopts).merge(gs, gm)
  
  
  def test_edge_names(self):
    for e in self.graph.all_edges():
      self.assertTrue(e.rel in ['syn-msr', ''] or re.match('s\d+', e.rel))
  
  
  def test_node_types(self):
    for n in self.graph.all_nodes():
      if n.synset:
        self.assertIsInstance(n.synset.synset_id, int)
        self.assertIsInstance(n.synset.lu_set, set)
      elif n.msr:
        self.assertIsInstance(n.msr, str)
      else:
        self.fail('The node no %d is neither synset nor msr!' % int(n))
  
  
  def test_numbers(self):
    self.assertEquals(leniter(self.graph.all_nodes()), 18)
    self.assertEquals(leniter(self.graph.all_edges()), 16+11+7*2)



























