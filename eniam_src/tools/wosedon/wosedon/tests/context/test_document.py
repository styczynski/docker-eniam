# -*- coding: utf-8 -*-

import unittest
from wosedon.context.document import Document
import corpus2





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)




class TestSentence(unittest.TestCase):
  
  def setUp(self):
    tagset = corpus2.get_named_tagset('nkjp')
    cclreader = corpus2.CclRelReader(tagset, 'tests/#Data/wiki_ccl.xml', 'tests/#Data/wiki_ccl.xml')
    ccldoc = cclreader.read()
    self.document = Document(tagset, ccldoc, ccldoc.relations())
  
  
  def test_basics(self):
    dc = 0
    tc = 0
    
    doc = self.document.next()
    while doc:
      dc += 1
      for t in doc.tokens():
        tc += 1
      doc = self.document.next()
    
    self.assertEquals(dc, 1)
    self.assertEquals(tc, 411)
    
    self.document.reset()
    
    dc = 0
    tc = 0
    
    doc = self.document.next()
    while doc:
      dc += 1
      for t in doc.tokens():
        tc += 1
      doc = self.document.next()
    
    self.assertEquals(dc, 1)
    self.assertEquals(tc, 411)
  
  
  def test_wsd(self):
    doc = self.document.next()
    while doc:
      for t in doc.tokens():
        self.assertIsInstance(doc.get_token_lemma_str(t), str)
        self.assertIn(doc.get_token_coarse_pos(t), ['n', 'v', 'a', None])
      doc = self.document.next()





























