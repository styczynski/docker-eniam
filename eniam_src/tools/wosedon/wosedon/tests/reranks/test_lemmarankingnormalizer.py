import unittest
from wosedon.reranks import LemmaRankingNormalizer
from wosedon.basegraph import BaseGraph
from wosedon.utils.config import Config



class TestLemmaRankingNormalizer(unittest.TestCase):
  
  def setUp(self):
    cfg = Config()
    cfg.parse('tests/#Data/wosedon_cosine.ini')
    cfg.resources().set_tagset('nkjp')
    cfg._context = 'Document'
    ctx = cfg.context('tests/#Data/little_ccl.xml', 'tests/#Data/little_ccl.xml')
    
    g = BaseGraph()
    g.init_graph()
    self.nodes = []
    for i in xrange(10):
      self.nodes.append(g.add_node(i))
    
    self.tokens = ctx.next().tokens()
    self.graph = g
    self.rerank = LemmaRankingNormalizer()
    self.options = cfg.rerank_options()
  
  
  def test_it(self):
    ranking = [
      (self.tokens[0], [(self.nodes[0], 3), (self.nodes[1], 2), (self.nodes[2], 1)]),
      (self.tokens[1], [(self.nodes[3], 5)]),
      (self.tokens[2], [(self.nodes[4], 11), (self.nodes[5], 7)]),
      (self.tokens[3], [(self.nodes[6], 13)]),
      (self.tokens[4], [(self.nodes[7], 31), (self.nodes[8], 23), (self.nodes[9], 17)])
    ]
    
    self.assertAlmostEquals(self.rerank.rerank(ranking, self.graph, self.options),
      [
        (self.tokens[0], [(self.nodes[0], 0.5), (self.nodes[1], 0.3333333333333333), (self.nodes[2], 0.16666666666666666)]),
        (self.tokens[1], [(self.nodes[3], 1)]),
        (self.tokens[2], [(self.nodes[4], 0.6111111111111112), (self.nodes[5], 0.3888888888888889)]),
        (self.tokens[3], [(self.nodes[6], 1)]),
        (self.tokens[4], [(self.nodes[7], 0.43661971830985913), (self.nodes[8], 0.323943661971831), (self.nodes[9], 0.23943661971830985)])
      ])
    



