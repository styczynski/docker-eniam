# -*- coding: utf-8 -*-

import unittest
from wosedon.lesk_functions.example_function import ExampleFunction
from wosedon.utils.config import Config






class TestLeskIntersection(unittest.TestCase):

  def setUp(self):
    cfg = Config()
    cfg.parse('tests/#Data/wosedon_example.ini')
    
    self.graph = cfg.gbuilders()[0].build_graph()
    self.context = cfg.context('tests/#Data/wiki_ccl.xml', 'tests/#Data/wiki_ccl.xml')
    self.example = cfg.algorithm_options().lesk_function(cfg.resources(), self.graph)
  
  
  def test_intersection(self):
    
    definition_data = {
      ('aaaa', 'adv') : 1,
      ('bbbb', 'subst') : 2,
      ('ccccc', 'fin') : 3,
      ('ddd', 'adja') : 4,
      ('eee', 'depr') : 5,
      ('fff', 'inf') : 6,
      ('ggg', 'pact') : 7
    }
    
    context_data = {
      ('aaaa', 'adv') : 4,
      ('bbbb', 'subst') : 2,
      ('ccccc', 'fin') : 4,
      ('ddd', 'adja') : 5,
      ('eee', 'depr') : 6,
      ('fff', 'inf') : 2,
      ('ggg', 'pact') : 1,
      ('afsadfs', 'inf') : 1352352,
      ('hthhj', 'inf') : 356456464,
      ('ewrwre', 'inf') : 2352343
    }
    
    self.assertAlmostEquals(self.example.compare(definition_data, context_data), -20.60103894856)
  
  
  def test_run_prepare_node(self):
    self.graph.create_edge_attribute('weight', 'float')
    for node in self.graph.all_nodes():
      res = self.example.prepare_node(node)
      self.assertIsInstance(res, dict)
      for k in res:
        self.assertIsInstance(k, tuple)
        self.assertIsInstance(k[0], str)
        self.assertIsInstance(k[1], str)
        self.assertIsInstance(res[k], float)
  
  
  def test_run_prepare_context(self):
    sen = self.context.next()
    while sen:
      res = self.example.prepare_context(sen)
      self.assertIsInstance(res, dict)
      for k in res:
        self.assertIsInstance(k, tuple)
        self.assertIsInstance(k[0], str)
        self.assertIsInstance(k[1], str)
        self.assertIsInstance(res[k], float)
      sen = self.context.next()









