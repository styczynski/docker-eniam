# -*- coding: utf-8 -*-

import unittest, re
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.utils.mergeoptions import MergeOptions
from wosedon.gbuilders import SynsetGraphBuilder
from wosedon.gbuilders import WNDGraphBuilder
from wosedon.mergers import SynsetsWNDMerger2





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)



@unittest.skip("Mergers even don't know that duplicates should be removed.'")  #FIXME to powinno działać
class TestSynsetsWNDMerger2_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    res.set_wnd_graph_file('tests/#Data/wnd_graph')
    res.set_mapping_wnd_file('tests/#Data/wnd_mapping')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(False)
    bopts.set_unique_edges(True)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gw = WNDGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsWNDMerger2(res, mopts).merge(gs, gw)
  
  
  def test_undirected_unique(self):
    for s in self.graph.all_nodes():
      for t in self.graph.all_nodes():
        if s != t:
          if leniter(self.graph.get_edges_between(s, t)) > 1:
            self.fail((s, t))
          self.assertLessEqual(leniter(self.graph.get_edges_between(s, t)), 1)
        



class TestSynsetsWNDMerger2(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    res.set_wnd_graph_file('tests/#Data/wnd_graph')
    res.set_mapping_wnd_file('tests/#Data/wnd_mapping')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(True)
    bopts.set_unique_edges(False)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gw = WNDGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsWNDMerger2(res, mopts).merge(gs, gw)
  
  
  def test_edge_names(self):
    for e in self.graph.all_edges():
      self.assertTrue(e.rel in ['isa', 'syn-wnd'] or re.match('s\d+', e.rel))
  
  
  def test_node_types(self):
    for n in self.graph.all_nodes():
      self.assertIsInstance(n.synset.synset_id, int)
      self.assertIsInstance(n.synset.lu_set, set)
  
  
  def test_numbers(self):
    self.assertEquals(leniter(self.graph.all_nodes()), 11)
    self.assertEquals(leniter(self.graph.all_edges()), 32)



























