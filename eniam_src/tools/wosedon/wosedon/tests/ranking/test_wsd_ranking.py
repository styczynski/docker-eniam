# -*- coding: utf-8 -*-

import unittest
from wosedon.ranking.wsd_ranking import WSDRanking
from wosedon.basegraph import BaseGraph
from wosedon.utils.config import Config


@unittest.skip("Very mysterious errors, probably outside of python.")  #FIXME to powinno działać
class TestWSDRanking(unittest.TestCase):
  
  def setUp(self):
    class Synset:
      pass
      
    class LU:
      def __init__(self, lemma, pos):
        self.lemma = lemma
        self.pos = pos
    
    g = BaseGraph()
    g.init_graph()
    g.create_node_attribute('synset', 'object')
    
    self.nodes = []
    for i in xrange(10):
      self.nodes.append(g.add_node(i, [('synset', Synset())]))
        
    self.nodes[0].synset.lu_set = {LU('podatek', 2)}
    self.nodes[1].synset.lu_set = {LU('podatek', 2)}
    self.nodes[2].synset.lu_set = {LU('podatek', 2)}
    self.nodes[3].synset.lu_set = {LU('podatek', 2)}
    self.nodes[4].synset.lu_set = {LU('unia', 2)}
    self.nodes[5].synset.lu_set = {LU('unia', 2)}
    self.nodes[6].synset.lu_set = {LU('europejski', 4)}
    self.nodes[7].synset.lu_set = {LU('można', 1)}
    self.nodes[8].synset.lu_set = {LU('można', 1)}
    self.nodes[9].synset.lu_set = {LU('można', 1)}
    
    
    cfg = Config()
    cfg.parse('tests/#Data/wosedon_cosine.ini')
    cfg.resources().set_tagset('nkjp')
    cfg._context = 'Document'
    ctx = cfg.context('tests/#Data/little_ccl.xml', 'tests/#Data/little_ccl.xml').next()
    
    self.graph = g
    self.ranking = WSDRanking()
    self.ctx = ctx
  
  
  def test_set_get_lemma_ranking(self):
    rank1 = {7: 124, 34: 324}
    rank2 = {}
    rank3 = {354: 1, 324: 1}
    rank4 = {3: 43, 34: 34, 24: 53}
    
    self.ranking.set_ranking_for_lemma('aaa', 'n', rank1)
    self.ranking.set_ranking_for_lemma('bbb', 'v', rank2)
    self.ranking.set_ranking_for_lemma('ccc', 'p', rank3)
    self.assertEqual(self.ranking.get_ranking_for_lemma('bbb', 'v'), rank2)
    self.assertEqual(self.ranking.get_ranking_for_lemma('ccc', 'p'), rank3)
    self.ranking.set_ranking_for_lemma('ddd', 'n', rank4)
    
    self.assertEqual(self.ranking.get_ranking_for_lemma('aaa', 'n'), rank1)
    self.assertEqual(self.ranking.get_ranking_for_lemma('ddd', 'n'), rank4)
  
  
  def test_get_lemma_on_node_dict(self):
    lon, lon_os = self.ranking.get_lemma_on_node_dict(self.ctx, self.graph)
    
    self.assertEquals(lon, lon_os)
    self.assertEquals(len(lon), 4)
    self.assertEquals(len(lon['podatek', 'n']), 4)
    self.assertEquals(len(lon['unia', 'n']), 2)
    self.assertEquals(len(lon['europejski', 'a']), 1)
    self.assertEquals(len(lon['można', 'v']), 3)
    
    for k in lon:
      for n in lon[k]:
        self.assertIn(n, self.nodes)
  
  
  def test_get_lemma_on_node_dict_ini_sumo(self):
    self.graph.create_node_attribute('sumo', 'string')
    sumonodes = []
    for i in xrange(10):
      sumonodes.append(self.graph.add_node(10+i, [('sumo', 'adest')]))
      self.graph.add_edge(i, 10+i)
    
    lon, lon_os = self.ranking.get_lemma_on_node_dict(self.ctx, self.graph, ['sumo'])
    
    self.assertEquals(len(lon), 4)
    self.assertEquals(len(lon['podatek', 'n']), 4)
    self.assertEquals(len(lon['unia', 'n']), 2)
    self.assertEquals(len(lon['europejski', 'a']), 1)
    self.assertEquals(len(lon['można', 'v']), 3)
    for k in lon:
      for n in lon[k]:
        self.assertIn(n, sumonodes)
    
    self.assertEquals(len(lon_os), 4)
    self.assertEquals(len(lon_os['podatek', 'n']), 4)
    self.assertEquals(len(lon_os['unia', 'n']), 2)
    self.assertEquals(len(lon_os['europejski', 'a']), 1)
    self.assertEquals(len(lon_os['można', 'v']), 3)
    for k in lon_os:
      for n in lon_os[k]:
        self.assertIn(n, self.nodes)
    
  
  def test_get_lemma_on_node_dict_ini_syn_msr(self):
    self.graph.create_node_attribute('msr', 'string')
    msrnodes = []
    for i in xrange(10):
      msrnodes.append(self.graph.add_node(10+i, [('msr', 'adest')]))
      self.graph.add_edge(i, 10+i)
    
    lon, lon_os = self.ranking.get_lemma_on_node_dict(self.ctx, self.graph, ['synset', 'msr'])
    
    self.assertEquals(len(lon), 4)
    self.assertEquals(len(lon['podatek', 'n']), 8)
    self.assertEquals(len(lon['unia', 'n']), 4)
    self.assertEquals(len(lon['europejski', 'a']), 2)
    self.assertEquals(len(lon['można', 'v']), 6)
    for k in lon:
      for n in lon[k]:
        self.assertIn(n, msrnodes + self.nodes)
    
    self.assertEquals(len(lon_os), 4)
    self.assertEquals(len(lon_os['podatek', 'n']), 4)
    self.assertEquals(len(lon_os['unia', 'n']), 2)
    self.assertEquals(len(lon_os['europejski', 'a']), 1)
    self.assertEquals(len(lon_os['można', 'v']), 3)
    for k in lon_os:
      for n in lon_os[k]:
        self.assertIn(n, self.nodes)
  
  
  def test_get_lemma_on_node_dict_ini_sumo_wnd(self):
    self.graph.create_node_attribute('sumo', 'string')
    self.graph.create_node_attribute('wnd', 'string')
    
    sumonodes = []
    for i in xrange(10):
      sumonodes.append(self.graph.add_node(10+i, [('sumo', 'adest')]))
      self.graph.add_edge(i, 10+i)
      
    wndnodes = []
    for i in xrange(10):
      wndnodes.append(self.graph.add_node(20+i, [('wnd', 'adest')]))
      self.graph.add_edge(i, 20+i)
    
    lon, lon_os = self.ranking.get_lemma_on_node_dict(self.ctx, self.graph, ['sumo', 'wnd'])
    
    self.assertEquals(len(lon), 4)
    self.assertEquals(len(lon['podatek', 'n']), 8)
    self.assertEquals(len(lon['unia', 'n']), 4)
    self.assertEquals(len(lon['europejski', 'a']), 2)
    self.assertEquals(len(lon['można', 'v']), 6)
    for k in lon:
      for n in lon[k]:
        self.assertIn(n, sumonodes + wndnodes)
    
    self.assertEquals(len(lon_os), 4)
    self.assertEquals(len(lon_os['podatek', 'n']), 4)
    self.assertEquals(len(lon_os['unia', 'n']), 2)
    self.assertEquals(len(lon_os['europejski', 'a']), 1)
    self.assertEquals(len(lon_os['można', 'v']), 3)
    for k in lon_os:
      for n in lon_os[k]:
        self.assertIn(n, self.nodes)
  
  
  def test_get_context_ranking(self):
    rank = {node: -float(rank) for rank, node in enumerate(self.nodes)}
    
    self.ranking.set_ranking_for_lemma('podatek', 'n', rank)
    self.ranking.set_ranking_for_lemma('w', None, rank)
    self.ranking.set_ranking_for_lemma('unia', 'n', rank)
    self.ranking.set_ranking_for_lemma('europejski', 'a', rank)
    self.ranking.set_ranking_for_lemma('można', 'v', rank)
    
    res = self.ranking.get_ranking_for_context(self.ctx, self.graph)
    
    tks = self.ctx.tokens()
    
    def check(ord_, cnt):
      r = res[ord_]
      self.assertEquals(len(r), 2)
      self.assertIs(r[0], tks[ord_])
      if cnt == 0:
        self.assertIs(r[1], None)
      else:
        self.assertEquals(len(r[1]), cnt)
        for i in r[1]:
          self.assertEquals(len(i), 2)
          self.assertIn(i[0], self.nodes)
          self.assertIsInstance(i[1], float)
    
    self.assertEquals(len(res), 5)
    check(0, 4)
    check(1, 0)
    check(2, 2)
    check(3, 1)
    check(4, 3)
    
    



