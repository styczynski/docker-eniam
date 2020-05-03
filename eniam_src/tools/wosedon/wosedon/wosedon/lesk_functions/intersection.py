from wosedon.lesk_functions.dictionarybased import DictionaryBased



class Intersection(DictionaryBased):
  """! How much definition intersects with context. """
  
  def __init__(self, resources, filter_obj, graph, options, str_name='Intersection'):
    super(Intersection, self).__init__(resources, filter_obj, graph, options, str_name)
  
  def compare(self, definition_data, context_data):
    coverage = 0
    for (lemma, pos_str) in definition_data:
      if (lemma, pos_str) in context_data:
        coverage += 1
    return float(coverage)





















