# -*- coding: utf-8 -*-

import unittest
from wosedon.context.sentence import Sentence
import corpus2





def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)




class TestSentence(unittest.TestCase):
  
  def setUp(self):
    tagset = corpus2.get_named_tagset('nkjp')
    cclreader = corpus2.CclRelReader(tagset, 'tests/#Data/wiki_ccl.xml', 'tests/#Data/wiki_ccl.xml')
    ccldoc = cclreader.read()
    self.sentence = Sentence(tagset, ccldoc, ccldoc.relations())
  
  
  def test_basics(self):
    sc = 0
    tc = 0
    
    sen = self.sentence.next()
    while sen:
      sc += 1
      for t in sen.tokens():
        tc += 1
      sen = self.sentence.next()
    
    self.assertEquals(sc, 20)
    self.assertEquals(tc, 411)
    
    self.sentence.reset()
    
    sc = 0
    tc = 0
    
    sen = self.sentence.next()
    while sen:
      sc += 1
      for t in sen.tokens():
        tc += 1
      sen = self.sentence.next()
    
    self.assertEquals(sc, 20)
    self.assertEquals(tc, 411)
  
  
  def test_wsd(self):
    sen = self.sentence.next()
    while sen:
      for t in sen.tokens():
        self.assertIsInstance(sen.get_token_lemma_str(t), str)
        self.assertIn(sen.get_token_coarse_pos(t), ['n', 'v', 'a', None])
      sen = self.sentence.next()



























