from wosedon.reranks.rerankinterface import RerankInterface
import operator

class AContentReranker(RerankInterface):
  def __init__(self, str_name = 'AContentReranker'):
    super(AContentReranker, self).__init__(str_name)

  def rerank(self, ranking, graph, options):
    new_ranking = []
    for (token, token_rank) in ranking:
      if not token_rank:
        new_ranking.append((token, token_rank))
        continue

      new_token_rank = []
      node = token_rank[0][0]
      for ngb in node.all_neighbours():
        lu_set = ngb.synset.lu_set
        lexicon = self._get_most_frequent_lexicon(graph, lu_set)
        if lexicon == "AContent_1.0":
          new_token_rank.append((ngb, 0.0))
      new_ranking.append((token, new_token_rank))

    return new_ranking

  def _get_most_frequent_lexicon(self, graph, lu_set):
    lexicon_dict = {}
    for lu in lu_set:
      if lu.lexicon not in lexicon_dict:
        lexicon_dict[lu.lexicon] = 0
      lexicon_dict[lu.lexicon] += 1
    return max(lexicon_dict.iteritems(), key=operator.itemgetter(1))[0]
