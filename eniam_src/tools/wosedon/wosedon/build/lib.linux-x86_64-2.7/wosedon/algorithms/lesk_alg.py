from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.ranking.wsd_ranking import WSDRanking

class LeskAlg(WSDAlgorithmInterface):
  """!
  The Lesk algorithm is based on the assumption that words in a given
  "neighborhood" (section of text) will tend to share a common topic.
  This version of the algorithm allows to use chosen functions for comparison
  between definition of ambiguous word and its neighborhood.
  """

  def __init__(self, str_name = 'LeskAlg'):
    super(LeskAlg, self).__init__(str_name)
    self.node_data = {}  # {synset_id : definition data returned by function}

  def prepare_v(self, wsd_context, graph):
    """! Required by WSDAlgorithmInterface. """
    pass

  def run(self, wsd_context, graph, options, resources):
    """!
    Disambiguate analyzed context.

    @type wsd_context: WSDContext
    @type graph: BaseGraph
    @type options: AlgorithmOptions
    @type resources: Resources
    
    @rtype (WSDRanking, int)
    @return: tuple containing WSDRanking and number of algorithm iterations
    """
    
    function = options.lesk_function(resources, graph)
    context_data = function.prepare_context(wsd_context)
    
    wsd_rank = WSDRanking()
    (lemma_on_node_dict, lemma_on_only_synset_node_dict) = \
      wsd_rank.get_lemma_on_node_dict(wsd_context, graph, options.ini_nodes())
    
    func_cache = {}
    
    for (lemma, pos_str), nodes in lemma_on_node_dict.iteritems():
      for node in nodes:
        # TODO It assumes that there exists exactly one graph during whole program, which is true for now.
        if node not in self.node_data:
          self.node_data[node] = function.prepare_node(node)
        
        if node not in func_cache:
          func_cache[node] = function.compare(
              self.node_data[node],
              context_data
          )
      
      wsd_rank.set_ranking_for_lemma(lemma, pos_str, func_cache, create_new = False)
    
    return (wsd_rank, 0)








