import math

from wosedon.lesk_functions.dictionarybased import DictionaryBased


def _cosine_similarity(v1, v2):
  sumxx, sumxy, sumyy = 0, 0, 0
  for i in xrange(len(v1)):
    x = v1[i]; y = v2[i]
    sumxx += x*x
    sumyy += y*y
    sumxy += x*y
  if sumxx <= 0 or sumyy <= 0:
    return 0.0
  return sumxy/math.sqrt(sumxx*sumyy)



class Cosine(DictionaryBased):
  """! Vector similarity between definition and context. """
  
  def __init__(self, resources, filter_obj, graph, options, str_name='Cosine'):
    super(Cosine, self).__init__(resources, filter_obj, graph, options, str_name)
  
  def compare(self, definition_data, context_data):
    vec1 = []
    vec2 = []
    for (lemma, pos_str) in definition_data:
      vec1.append(definition_data[(lemma, pos_str)] / float(len(definition_data)))
      if (lemma, pos_str) in context_data:
        vec2.append(context_data[(lemma, pos_str)] / float(len(context_data)))
      else:
        vec2.append(0)
    
    return _cosine_similarity(vec1, vec2)





















