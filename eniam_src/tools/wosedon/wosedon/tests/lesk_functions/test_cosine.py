# -*- coding: utf-8 -*-

import unittest
from wosedon.lesk_functions.cosine import _cosine_similarity, Cosine
from wosedon.utils.config import Config





class TestCosineSimilarity(unittest.TestCase):

  def test_cosine_function(self):
    self.assertAlmostEquals(_cosine_similarity(
                      [2, 24, 3, 64, -2, -5], [2, 24, 3, 64, -2, -5]), 1)
    self.assertAlmostEquals(_cosine_similarity(
                      [-1, 2], [1, 2]), 0.6)
    self.assertAlmostEquals(_cosine_similarity(
                      [17, 213, 33, 11, 3453], [17, 34, 11, 33, 456]), 0.996929679772)
    self.assertAlmostEquals(_cosine_similarity(
                      [3, -2], [2, 3]), 0)
    self.assertAlmostEquals(_cosine_similarity(
                      [1, 2, 3, 4, 5, 6, 7], [4, 2, 4, 5, 6, 2, 1]), 0.744776393793)
    self.assertAlmostEquals(_cosine_similarity(
                      [1, 2, 3], [0, 0, 0]), 0)
    




class TestLeskCosine(unittest.TestCase):

  def setUp(self):
    cfg = Config()
    cfg.parse('tests/#Data/wosedon_cosine.ini')
    
    self.graph = cfg.gbuilders()[0].build_graph()
    self.context = cfg.context('tests/#Data/wiki_ccl.xml', 'tests/#Data/wiki_ccl.xml')
    self.cosine = cfg.algorithm_options().lesk_function(cfg.resources(), self.graph)
  
  
  def test_cosine(self):
    
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
    
    self.assertAlmostEquals(self.cosine.compare(definition_data, context_data), 0.744776393793)
  
  
  def test_run_prepare_node(self):
    self.graph.create_edge_attribute('weight', 'float')
    for node in self.graph.all_nodes():
      res = self.cosine.prepare_node(node)
      self.assertIsInstance(res, dict)
      for k in res:
        self.assertIsInstance(k, tuple)
        self.assertIsInstance(k[0], str)
        self.assertIsInstance(k[1], str)
        self.assertIsInstance(res[k], float)
  
  
  def test_run_prepare_context(self):
    sen = self.context.next()
    while sen:
      res = self.cosine.prepare_context(sen)
      self.assertIsInstance(res, dict)
      for k in res:
        self.assertIsInstance(k, tuple)
        self.assertIsInstance(k[0], str)
        self.assertIsInstance(k[1], str)
        self.assertIsInstance(res[k], float)
      sen = self.context.next()









