import sys, logging
from wosedon.reranks.rerankinterface import RerankInterface

class LemmaRankingSelecter(RerankInterface):
  """Dla lematow sprawdzana jest roznica pomiedzy wartoscia 
  rankingu pierwszego synsetu, a pozostalymi synsetami. Jezeli
  roznica jest mniejsza niz zadana w pliku konfiguracyjnym
  wartosc percentage_diff, to wybierany jest pomiedzy nimi synset, ktory
  ma najmniejszy wariant."""
  
  def __init__(self, str_name = 'LemmaRankingSelecter'):
    super(LemmaRankingSelecter, self).__init__(str_name)

  def rerank(self, ranking, graph, options):
    logger = logging.getLogger(__name__)
    
    
    tagset = options.tagset()

    new_ranking = []
    for (token, token_rank) in ranking:
      if not token_rank:
        new_ranking.append([token, token_rank])
        continue

      lemma = str(token.get_preferred_lexeme(tagset).lemma())

      rank_sum = sum(rank for (node, rank) in token_rank)
      percentage_diff_value = (float(options.percentage_diff()) * float(rank_sum)) / 100.0
      best_value = token_rank[0][1]

      token_rank_dict = dict(((node, rank), i) for i, (node, rank) in enumerate(token_rank))
      selected_synsets_dict = {}

      for (node, rank), i in token_rank_dict.iteritems():
        diff = abs(best_value - rank)
        if diff <= percentage_diff_value:
          synset_variant = self.get_synset_variant(lemma, node, graph)

          if not synset_variant:
            logger.warning('Lemmas in the synset are completely different from the lemma of the token.')
            continue

          selected_synsets_dict[(node, rank)] = synset_variant

      if len(selected_synsets_dict) == 1:
        new_ranking.append([token, token_rank])
        continue

      new_token_rank = sorted(selected_synsets_dict, key=selected_synsets_dict.get)

      for (node, rank), i in selected_synsets_dict.iteritems():
        del token_rank_dict[(node, rank)]

      new_token_rank.extend(sorted(token_rank_dict, key=token_rank_dict.get))

      new_ranking.append((token, new_token_rank))

    return new_ranking

  def get_synset_variant(self, lemma, node, graph):
    variant = 0
    for lu in node.synset.lu_set:
      if lu.lemma == lemma:
        variant = lu.variant
        break
    return variant
