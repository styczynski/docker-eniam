# -*- coding: utf-8 -*-

import unittest
from wosedon.lesk_filters import No
import corpus2




def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestLeskFilterNo(unittest.TestCase):

  def setUp(self):
    self.filter = No(corpus2.get_named_tagset('nkjp'), {})

  def test_it(self):
    self.assertNotIn('abcd', self.filter)
    self.assertNotIn('fgdesrg', self.filter)
    self.assertNotIn('ab24232cd', self.filter)
    self.assertNotIn('8ki67j', self.filter)
    self.assertNotIn('rtert', self.filter)
    self.assertNotIn('232 4 345', self.filter)



