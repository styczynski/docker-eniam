from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface


class Words(LeskFilterInterface):
  """! Allows only some words. """
  
  def __init__(self, tagset, options, str_name='Words'):
    super(Words, self).__init__(tagset, options, str_name)
    
    self._allow_only = options.allow_only()
    self._set = set()
    
    if options.input_file():
      with open(options.input_file()) as f:
        for line in f:
          line = line.strip()
          self._set.add(line)
      
  
  def __contains__(self, lexeme):
    lemma = str(lexeme.lemma())
    return self._allow_only == (lemma in self._set)




