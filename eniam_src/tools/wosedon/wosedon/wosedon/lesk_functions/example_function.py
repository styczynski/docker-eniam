from wosedon.lesk_functions.dictionarybased import DictionaryBased



class ExampleFunction(DictionaryBased):
  
  def __init__(self, resources, filter_obj, graph, options, str_name='ExampleFunction'):
    super(ExampleFunction, self).__init__(resources, filter_obj, graph, options, str_name)
    
  
  def compare(self, definition_data, context_data):
    coverage = 0.0
    for (lemma, pos_str) in definition_data:
      if (lemma, pos_str) in context_data:
        coverage -= abs(definition_data[(lemma, pos_str)] - context_data[(lemma, pos_str)])**1.2
    return coverage
    
