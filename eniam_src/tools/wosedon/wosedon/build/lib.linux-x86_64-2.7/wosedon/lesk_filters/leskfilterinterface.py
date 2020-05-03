from abc import abstractmethod



class LeskFilterInterface(object):
  """! Interface that shall be implemented by all filters of Lesk. """
  
  def __init__(self, tagset, options, str_name):
    """!
    Standard constructor.
    
    @type tagset: corpus2.Tagset
    @param tagset: tagset
    
    @type options: dict[str, str]
    @param options: dictionary with options read from config file
    
    @type str_name: str
    @param str_name: name to be displayed, it should be name of class
    """
    self._str_name = str_name
    self.tagset = tagset

  def __str__(self):
    return self._str_name
  
  @abstractmethod
  def __contains__(self, lexeme):
    """!
    Check if lemma is allowed
    
    @type lexeme: corpus2.Lexeme
    @param lexeme: lexeme to be checked
    
    @rtype: bool
    @return: True if lexeme is allowed
    """
    raise NotImplementedError("LeskFunctionInterface!")













