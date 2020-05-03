# -*- coding: utf-8 -*-

import unittest
from wosedon.lesk_filters import Words
from wosedon.utils.leskfilteroptions import LeskFilterOptions

import corpus2
from wosedon.context.document import Document




def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestLeskFilterWords(unittest.TestCase):

  def setUp(self):
    self.tagset = corpus2.get_named_tagset('nkjp')
    
    cclreader = corpus2.CclRelReader(self.tagset, 'tests/#Data/wiki_ccl.xml', 'tests/#Data/wiki_ccl.xml')
    ccldoc = cclreader.read()
    self.document = Document(self.tagset, ccldoc, ccldoc.relations())


  def test_it(self):
    opts = LeskFilterOptions()
    opts.set_input_file('tests/#Data/leskfilters/words')
    self.filter = Words(self.tagset, opts)
    
    doc = self.document.next()
    while doc:
      for t in doc.tokens():
        lexeme = t.get_preferred_lexeme(self.tagset)
        lemma = str(lexeme.lemma())
        if lemma in ['z', 'kot', 'robić']:
          self.assertNotIn(lexeme, self.filter)
        else:
          self.assertIn(lexeme, self.filter)
          
      doc = self.document.next()

  
  def test_reversed(self):
    opts = LeskFilterOptions()
    opts.set_input_file('tests/#Data/leskfilters/words')
    opts.set_allow_only(True)
    self.filter = Words(self.tagset, opts)
    
    doc = self.document.next()
    while doc:
      for t in doc.tokens():
        lexeme = t.get_preferred_lexeme(self.tagset)
        lemma = str(lexeme.lemma())
        if lemma in ['z', 'kot', 'robić']:
          self.assertIn(lexeme, self.filter)
        else:
          self.assertNotIn(lexeme, self.filter)
          
      doc = self.document.next()



