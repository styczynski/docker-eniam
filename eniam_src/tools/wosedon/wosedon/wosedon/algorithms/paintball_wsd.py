from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.ranking.wsd_ranking import WSDRanking
from collections import deque, defaultdict


class PaintballWSD(WSDAlgorithmInterface):

  def __init__(self, str_name = 'PaintballWSD'):
    """!
    @param str_name - default value same as the class name "PaintballWSD"
    """
    super(PaintballWSD, self).__init__(str_name)


  def prepare_v(self, wsd_context, graph):
    """!
    Required by WSDAlgorithmInterface.
    """
    pass
  
  
  def make_original_values(self, lemma_on_node_dict):
    vals = {}
    for nodes in lemma_on_node_dict.itervalues():
      for node in nodes:
        if node in vals:
          vals[node] += self._multiply_factor / float(len(nodes))
        else:
          vals[node] = self._multiply_factor / float(len(nodes))
    return vals
  
  
  def BFS(self, start, original_value, values, impedance_table):
    Q = deque()
    values[start] += original_value
    visited = set()
    visited.add(start)
    
    for edge in start.all_edges():
      if edge.target() != start:
        Q.append((edge, original_value * edge.weight * self.damping, 1))
        visited.add(edge.target())
    
    while Q:
      e, v, d = Q.popleft()
      values[e.target()] += v
      if d < self.iter:
        for e2 in e.target().all_edges():
          if e2.target() not in visited:
            Q.append((e2, v * self.damping * e2.weight * self.impedance_table[e.rel][e2.rel], d + 1))
            visited.add(e2.target())
      
  
  
  def run(self, wsd_context, graph, options, resources):
    """!
    Disambiguate analyzed context.

    @param wsd_context - object of WSDContext class
    @param graph - object of BaseGraph class
    @param options - object of AlgorithmOptions class
    @param resources - object of Resources class
    @return tuple of object of WSDRanking class and number of algorithm iterations
    """
    self.impedance_table = resources.impedance_table()
    self.iter = options.max_iter()
    self.damping = options.damping_factor()
    
    
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())
    
    original_values = self.make_original_values(lemma_on_node_dict)
    values = defaultdict(float)
    
    for node, original_value in original_values.iteritems():
      self.BFS(node, original_value, values, resources.impedance_table)

    for (lemma, pos_str) in lemma_on_only_synset_node_dict.iterkeys():
      wsd_rank.set_ranking_for_lemma(lemma, pos_str, values)

    return (wsd_rank, self.iter)
    
