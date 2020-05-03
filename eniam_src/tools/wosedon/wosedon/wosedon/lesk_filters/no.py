from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface



class No(LeskFilterInterface):
  """! Forbids everything, probably is useless. """
  
  def __init__(self, tagset, options, str_name='No'):
    super(No, self).__init__(tagset, options, str_name)
  
  def __contains__(self, lexeme):
    return False
