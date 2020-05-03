from wosedon.lesk_functions.leskfunctioninterface import LeskFunctionInterface
from collections import deque

  
def _mult_and_add_to(dict1, dict2, c):
  for k in dict2:
    if k in dict1:
      dict1[k] += dict2[k] * c
    else:
      dict1[k] = dict2[k] * c



class DictionaryBased(LeskFunctionInterface):
  """!
  Prepares dictionaries {(lemma, pos) : importance} both for context
  and definition.
  """
  
  def __init__(self, resources, filter_obj, graph, options, str_name):
    super(DictionaryBased, self).__init__(resources, filter_obj, graph, options, str_name)
    
    self.definition_cache = {}
    self.damping = self.options.damping_factor()
    self.iter = self.options.max_iter()
    
  
  
  def _get_index(self, token):
    """!
    Convert token into pair (lemma, part of speech), which can be used as index
    in being prepared dictionaries.
    """
    lexeme = token.get_preferred_lexeme(self.tagset)
    if lexeme in self.filter_obj:
      lemma = str(lexeme.lemma())
      pos_str = self.tagset.get_pos_name(lexeme.tag().get_pos_index())
      return lemma, pos_str
    return None


  def _prepare_definition(self, synset_id):
    if not synset_id in self.definition_cache:
      if synset_id in self.gloss_dict:
        definition = self.gloss_dict[synset_id]
        res = {}
        for sentence in definition.sentences():
          for token in sentence.tokens():
            ind = self._get_index(token)
            res[ind] = 1.0
              
        if None in res:
          del res[None]
        self.definition_cache[synset_id] = res
      else:
        self.definition_cache[synset_id] = {}
    return self.definition_cache[synset_id]
  
  
  def prepare_node(self, node):
    res = {}
    
    q = deque()
    visited = set()
    
    q.append((node, 0, 1.0))
    visited.add(node)
    
    while q:
      v, d, w = q.popleft()
      
      _mult_and_add_to(res,  self._prepare_definition(int(v.synset.synset_id)), w)
      
      if d < self.iter:
        for e in v.all_edges():
          if e.target() not in visited:
            q.append((e.target(), d+1, w * e.weight * self.damping))
            visited.add(e.target())
    
    return res
  
    
  def prepare_context(self, context):
    res = {}
    
    for token in context.tokens():
      ind = self._get_index(token)
      if ind in res:
          res[ind] += 1.0
      else:
          res[ind] = 1.0
    
    if None in res:
      del res[None]
    return res












