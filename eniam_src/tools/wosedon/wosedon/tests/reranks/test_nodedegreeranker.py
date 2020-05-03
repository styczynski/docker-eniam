import unittest
from wosedon.reranks import NodeDegreeRanker
from wosedon.basegraph import BaseGraph
from wosedon.utils.config import Config



class TestNodeDegreeRanker(unittest.TestCase):
  
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
    self.rerank = NodeDegreeRanker()
    self.options = cfg.rerank_options()
  
  
  def test_it(self):
    ranking = [
      (self.tokens[0], [(self.nodes[0], 3), (self.nodes[1], 2), (self.nodes[2], 1)]),
      (self.tokens[1], [(self.nodes[3], 5)]),
      (self.tokens[2], [(self.nodes[4], 11), (self.nodes[5], 7)]),
      (self.tokens[3], [(self.nodes[6], 13)]),
      (self.tokens[4], [(self.nodes[7], 31), (self.nodes[8], 23), (self.nodes[9], 17)])
    ]
    
    self.assertEquals(self.rerank.rerank(ranking, self.graph, self.options),
      [
        (self.tokens[0], [(self.nodes[0], 0), (self.nodes[1], 0), (self.nodes[2], 0)]),
        (self.tokens[1], [(self.nodes[3], 0)]),
        (self.tokens[2], [(self.nodes[4], 0), (self.nodes[5], 0)]),
        (self.tokens[3], [(self.nodes[6], 0)]),
        (self.tokens[4], [(self.nodes[7], 0), (self.nodes[8], 0), (self.nodes[9], 0)])
      ])
    
    self.graph.add_edge(0, 1)
    self.graph.add_edge(1, 2)
    
    self.graph.add_edge(3, 4)
    self.graph.add_edge(3, 5)
    self.graph.add_edge(3, 6)
    
    new_rank = self.rerank.rerank(ranking, self.graph, self.options)
    
    self.assertEquals(len(new_rank), 5)
    for i, (tok, rank) in enumerate(new_rank):
      self.assertIs(tok, self.tokens[i])
    
    self.assertEquals(new_rank[0][1][0][0], self.nodes[1])
    self.assertEquals(new_rank[1][1][0][0], self.nodes[3])
    self.assertEquals(new_rank[2][1][0][0], self.nodes[4])
    self.assertEquals(new_rank[3][1][0][0], self.nodes[6])
    self.assertEquals(new_rank[4][1][0][0], self.nodes[7])
    



