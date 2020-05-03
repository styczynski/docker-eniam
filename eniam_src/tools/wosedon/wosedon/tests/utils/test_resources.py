import unittest
from wosedon.utils.resources import Resources

import corpus2
from collections import defaultdict




class TestResources(unittest.TestCase):
  
  def setUp(self):
    self.res = Resources()
  
  
  def test_tagset(self):
    # it's doubled in case it work only once
    self.res.set_tagset('nkjp')
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
    self.res.set_tagset('kipi')
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
    self.res.set_tagset('morfeusz2')
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
    self.assertIsInstance(self.res.tagset(), corpus2.Tagset)
  
  
  def test_nottagset(self):
    self.res.set_tagset('!@#$%^')
    self.assertRaises(IndexError, self.res.tagset)
    self.res.set_tagset(354)
    self.assertRaises(TypeError, self.res.tagset)
    self.res.set_tagset(['nkjp'])
    self.assertRaises(TypeError, self.res.tagset)
    
    
  def test_glossfile(self):
    self.res.set_tagset('nkjp')
    self.res.set_gloss_file('tests/#Data/glossfile.xml')
    gf = self.res.gloss_file()
    
    self.assertIsInstance(gf, dict)
    self.assertEquals(len(gf), 5)
    self.assertEquals(sorted(gf.keys()), [10, 14, 16, 17, 18])
    for i in gf:
      self.assertIsInstance(gf[i], corpus2.ChunkPtr)
      self.assertIsInstance(gf[i], corpus2.ChunkPtr)
  
  
  def test_impedancetable(self):
    self.res.set_impedance_table('tests/#Data/impedance.csv')
    it = self.res.impedance_table()
    
    self.assertIsInstance(it, defaultdict)
    self.assertEquals(len(it), 4)
    for i in it:
      self.assertIsInstance(i, str)
      self.assertIsInstance(it[i], defaultdict)
      self.assertEquals(len(it[i]), 4)
      for j in it[i]:
        self.assertIsInstance(j, str)
        self.assertIsInstance(it[i][j], float)
    
    self.assertEquals(it[7][2], 0)
    












