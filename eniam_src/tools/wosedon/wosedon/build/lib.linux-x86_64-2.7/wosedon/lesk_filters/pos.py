from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface

class POS(LeskFilterInterface):
  """! Allows only some Parts of Speech """
  
  def __init__(self, tagset, options, str_name='POS'):
    super(POS, self).__init__(tagset, options, str_name)
    
    self._allow_only = options.allow_only()
    self._set = set()
    
    if options.input_file():
      with open(options.input_file()) as f:
        for line in f:
          line = line.strip()
          self._set.add(line)
      
  
  def __contains__(self, lexeme):
    pos = self.tagset.get_pos_name(lexeme.tag().get_pos_index())
    return self._allow_only == (pos in self._set)



