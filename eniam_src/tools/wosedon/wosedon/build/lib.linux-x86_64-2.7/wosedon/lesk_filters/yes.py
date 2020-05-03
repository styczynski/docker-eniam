from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface



class Yes(LeskFilterInterface):
  """! Allows everything. """
  
  def __init__(self, tagset, options, str_name='Yes'):
    super(Yes, self).__init__(tagset, options, str_name)
  
  def __contains__(self, lexeme):
    return True
