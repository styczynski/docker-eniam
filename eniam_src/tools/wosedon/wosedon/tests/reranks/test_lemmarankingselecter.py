# -*- coding: utf-8 -*-

import unittest
from wosedon.reranks import LemmaRankingSelecter
from wosedon.basegraph import BaseGraph
from wosedon.utils.config import Config



class TestLemmaRankingSelecter(unittest.TestCase):
  
  def setUp(self):
    class Synset:
      pass
    
    cfg = Config()
    cfg.parse('tests/#Data/wosedon_cosine.ini')
    cfg.resources().set_tagset('nkjp')
    cfg._context = 'Document'
    ctx = cfg.context('tests/#Data/little_ccl.xml', 'tests/#Data/little_ccl.xml')
    
    g = BaseGraph()
    g.init_graph()
    g.create_node_attribute('synset', 'object')
    
    self.nodes = []
    for i in xrange(10):
      self.nodes.append(g.add_node(i, [('synset', Synset())]))
    
    self.tokens = ctx.next().tokens()
    self.graph = g
    self.rerank = LemmaRankingSelecter()
    self.options = cfg.rerank_options()
  
  
  def test_it(self):
    class LU:
      def __init__(self, lemma, variant):
        self.lemma = lemma
        self.variant = variant
        
    self.nodes[0].synset.lu_set = {LU('podatek', 5)}
    self.nodes[1].synset.lu_set = {LU('podatek', 4)}
    self.nodes[2].synset.lu_set = {LU('podatek', 1)}
    self.nodes[3].synset.lu_set = {LU('w', 3)}
    self.nodes[4].synset.lu_set = {LU('unia', 2)}
    self.nodes[5].synset.lu_set = {LU('unia', 1)}
    self.nodes[6].synset.lu_set = {LU('europejski', 3)}
    self.nodes[7].synset.lu_set = {LU('można', 3)}
    self.nodes[8].synset.lu_set = {LU('można', 2)}
    self.nodes[9].synset.lu_set = {LU('można', 1)}
    
    ranking = [
      (self.tokens[0], [(self.nodes[0], 100), (self.nodes[1], 99), (self.nodes[2], 98)]),
      (self.tokens[1], [(self.nodes[3], 5)]),
      (self.tokens[2], [(self.nodes[4], 11), (self.nodes[5], 1)]),
      (self.tokens[3], [(self.nodes[6], 13)]),
      (self.tokens[4], [(self.nodes[7], 100), (self.nodes[8], 99), (self.nodes[9], 1)])
    ]
    
    new_rank = self.rerank.rerank(ranking, self.graph, self.options)
    
    self.assertEquals(len(new_rank), 5)
    for i, (tok, rank) in enumerate(new_rank):
      self.assertIs(tok, self.tokens[i])
    
    self.assertEquals(new_rank[0][1][0][0], self.nodes[2])
    self.assertEquals(new_rank[1][1][0][0], self.nodes[3])
    self.assertEquals(new_rank[2][1][0][0], self.nodes[4])
    self.assertEquals(new_rank[3][1][0][0], self.nodes[6])
    self.assertEquals(new_rank[4][1][0][0], self.nodes[8])
    



