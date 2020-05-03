from abc import abstractmethod



class LeskFunctionInterface(object):
  """! Interface that shall be implemented by all functions of Lesk. """
  
  def __init__(self, resources, filter_obj, graph, options, str_name):  #TODO co znaczy "paragraph object from Corpus2"?
    """!
    Standard constructor.
    
    @type tagset: corpus2.Tagset
    @param tagset: tagset
    
    @type filter_obj: Filter
    @param filter_obj: filter of unwanted words
    
    @type graph: graph_tool.Graph
    @param graph: graph
    
    @type gloss_dict: dict([int, paragraph object from Corpus2])
    @param gloss_dict: definitions for synsets
    
    @type options: AlgorithmOptions
    @param options: options for algorithms
    
    @type str_name: str
    @param str_name: name to be displayed, it should be name of class
    """
    self.options = options
    self.tagset = resources.tagset()
    self.graph = graph
    self.gloss_dict = resources.gloss_file()
    self.filter_obj = filter_obj
    self._str_name = str_name

  def __str__(self):
    return self._str_name

  def prepare_node(self, node):
    """!
    Prepare any data you like. It will be called at most once for each node.
    
    @type node: graph_tool.Vertex
    @param node: node from graph
    
    @rtype: object
    @return: whatever you like
    """
    return None
  
  def prepare_context(self, context):
    """!
    Prepare any data you like. It will be called at most once for each context.
    
    @type context: WSDContext
    @param context: context to be processed
    
    @rtype: object
    @return: whatever you like
    """
    return None
  
  @abstractmethod
  def compare(self, node_data, context_data):
    """!
    Check how well given definition matches context.
    
    @type node_data: object
    @param node_data: data created in prepare_node()
    
    @type context_data: object
    @param context_data: data created in prepare_context()
    
    @rtype: float
    @return: how likely it is that this definition be proper one
    """
    raise NotImplementedError("LeskFunctionInterface!")




