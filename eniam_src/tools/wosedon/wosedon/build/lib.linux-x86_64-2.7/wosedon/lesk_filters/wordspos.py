from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface

class WordsPOS(LeskFilterInterface):
  """!
  Allows only some words with proper part of Speech.
  
  It accepts file with following format:
  
  \code
    word; subst
    home; adj
    home; subst
    tie; *
    *; interp
  \endcode
    
  '*' means anything. "*;*" won't work.'
  """
  
  def __init__(self, tagset, options, str_name='WordsPOS'):
    super(WordsPOS, self).__init__(tagset, options, str_name)
    
    self._allow_only = options.allow_only()
    self._set = set()
    
    if options.input_file():
      with open(options.input_file()) as f:
        for line in f:
          line = line.split(';')
          self._set.add((line[0].strip(), line[1].strip()))
      
  
  def __contains__(self, lexeme):
    lemma = str(lexeme.lemma())
    pos = self.tagset.get_pos_name(lexeme.tag().get_pos_index())
    return self._allow_only == (((lemma, pos) in self._set) or \
                                  (('*', pos) in self._set) or \
                                ((lemma, '*') in self._set))



