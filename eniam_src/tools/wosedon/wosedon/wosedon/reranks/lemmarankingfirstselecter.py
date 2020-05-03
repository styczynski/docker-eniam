import sys
from wosedon.reranks.rerankinterface import RerankInterface

class LemmaRankingFirstSelecter(RerankInterface):
  """Dla lematow sprawdzana jest roznica pomiedzy wartoscia 
  rankingu pierwszego i drugiego synsetu. Jezeli
  roznica jest mniejsza niz zadana w pliku konfiguracyjnym
  wartosc, to do lematu przypisywany jest synset, ktory ma 
  najmiejszy wariant."""

  def __init__(self, str_name = 'LemmaRankingFirstSelecter'):
    super(LemmaRankingFirstSelecter, self).__init__(str_name)

  def rerank(self, ranking, graph, options):
    tagset = options.tagset()

    new_ranking = []
    for (token, token_rank) in ranking:
      if not token_rank:
        new_ranking.append([token, token_rank])
        continue

      if len(token_rank) == 1:
        new_ranking.append([token, token_rank])
        continue

      lemma = str(token.get_preferred_lexeme(tagset).lemma())

      rank_sum = sum(rank for (node, rank) in token_rank)
      percentage_diff_value = (float(options.percentage_diff()) * float(rank_sum)) / 100.0

      first_synset_rank = token_rank[0][1]
      second_synset_rank = token_rank[1][1]
      diff = abs(first_synset_rank - second_synset_rank)

      if diff <= percentage_diff_value:
        selected_element = self.get_element_of_the_highest_variant(token_rank, lemma, graph)
        token_rank.remove(selected_element)
        token_rank.insert(0, selected_element)

      new_ranking.append((token, token_rank))

    return new_ranking

  def get_element_of_the_highest_variant(self, token_rank, lemma, graph):
    element = None
    variant = sys.maxint
    for (node, rank) in token_rank:
      for lu in node.synset.lu_set:
        if lu.lemma == lemma:
          temp_variant = lu.variant
          break
      if temp_variant < variant:
        element = (node, rank)
        variant = temp_variant
    return element
