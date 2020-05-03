# -*- coding: utf-8 -*-

import unittest
from wosedon.reranks import AContentReranker
from wosedon.basegraph import BaseGraph
from wosedon.utils.config import Config



class TestAContentReranker(unittest.TestCase):
  
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
    
    g.add_edge(0, 1)
    g.add_edge(2, 7)
    g.add_edge(1, 7)
    g.add_edge(4, 1)
    g.add_edge(4, 5)
    g.add_edge(4, 6)
    g.add_edge(6, 7)
    g.add_edge(7, 3)
    
    self.tokens = ctx.next().tokens()
    self.graph = g
    self.rerank = AContentReranker()
    self.options = cfg.rerank_options()
  
  
  def test_it(self):
    class LU:
      def __init__(self, lemma, lexicon):
        self.lemma = lemma
        self.lexicon = lexicon
        
    self.nodes[0].synset.lu_set = {LU('podatek', 'Słowosieć_2.2')}
    self.nodes[1].synset.lu_set = {LU('podatek', 'AContent_1.0')}
    self.nodes[2].synset.lu_set = {LU('podatek', 'AContent_1.0')}
    self.nodes[3].synset.lu_set = {LU('w', 'Słowosieć_2.2')}
    self.nodes[4].synset.lu_set = {LU('unia', 'Słowosieć_2.2')}
    self.nodes[5].synset.lu_set = {LU('unia', 'AContent_1.0')}
    self.nodes[6].synset.lu_set = {LU('europejski', 'Słowosieć_2.2')}
    self.nodes[7].synset.lu_set = {LU('można', 'AContent_1.0')}
    self.nodes[8].synset.lu_set = {LU('można', 'Słowosieć_2.2')}
    self.nodes[9].synset.lu_set = {LU('można', 'AContent_1.0')}
    
    ranking = [
      (self.tokens[0], [(self.nodes[0], 100), (self.nodes[1], 99), (self.nodes[2], 98)]),
      (self.tokens[1], [(self.nodes[3], 5)]),
      (self.tokens[2], [(self.nodes[4], 11), (self.nodes[5], 1)]),
      (self.tokens[3], [(self.nodes[6], 13)]),
      (self.tokens[4], [(self.nodes[7], 100), (self.nodes[8], 99), (self.nodes[9], 98)])
    ]
    
    new_rank = self.rerank.rerank(ranking, self.graph, self.options)
    
    self.assertEquals(len(new_rank), 5)
    for i, (tok, rank) in enumerate(new_rank):
      self.assertIs(tok, self.tokens[i])
    
    self.assertEquals(new_rank[0][1][0][0], self.nodes[1])
    self.assertEquals(new_rank[1][1][0][0], self.nodes[7])
    self.assertEquals(new_rank[2][1][0][0], self.nodes[1])
    self.assertEquals(new_rank[3][1][0][0], self.nodes[7])
    self.assertEquals(new_rank[4][1][0][0], self.nodes[2])
    



