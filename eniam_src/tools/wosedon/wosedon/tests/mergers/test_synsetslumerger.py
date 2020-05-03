# -*- coding: utf-8 -*-

import unittest, re
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.utils.mergeoptions import MergeOptions
from wosedon.gbuilders import SynsetGraphBuilder
from wosedon.gbuilders import LexicalUnitGraphBuilder
from wosedon.mergers import SynsetsLUMerger





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)



class TestSynsetsLUMerger_DirectnessAndDuplicates(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(False)
    bopts.set_unique_edges(True)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gl = LexicalUnitGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsLUMerger(res, mopts).merge(gs, gl)
  
  
  def test_undirected_unique(self):
    for s in self.graph.all_nodes():
      for t in self.graph.all_nodes():
        if s != t:
          if leniter(self.graph.get_edges_between(s, t)) > 1:
            self.fail((s, t))
          self.assertLessEqual(leniter(self.graph.get_edges_between(s, t)), 1)
        



class TestSynsetsLUMerger(unittest.TestCase):
  
  def setUp(self):
    res = Resources()
    res.set_plwn_graph_file('tests/#Data/plwnik')
    
    bopts = BuildOptions()
    bopts.set_directed_graph(True)
    bopts.set_unique_edges(False)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gl = LexicalUnitGraphBuilder(res, bopts).build_graph()
    self.graph = SynsetsLUMerger(res, mopts).merge(gs, gl)
  
  
  def test_edge_names(self):
    for e in self.graph.all_edges():
      self.assertTrue(e.rel == 'syn-lu' or re.match('[sl]\d+', e.rel))
  
  
  def test_node_types(self):
    for n in self.graph.all_nodes():
      if n.synset:
        self.assertIsInstance(n.synset.synset_id, int)
        self.assertIsInstance(n.synset.lu_set, set)
      elif n.lu:
        self.assertIsInstance(n.lu.lu_id, int)
        self.assertIsInstance(n.lu.lemma, str)
      else:
        self.fail('The node no %d is neither synset nor lu!' % int(n))
  
  
  def test_numbers(self):
    self.assertEquals(leniter(self.graph.all_nodes()), 22)
    self.assertEquals(leniter(self.graph.all_edges()), 54)



























