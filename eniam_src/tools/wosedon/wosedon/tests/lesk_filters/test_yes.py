# -*- coding: utf-8 -*-

import unittest
from wosedon.lesk_filters import Yes
import corpus2




def leniter(i):
  """ Lenght of any iterable """
  return sum(1 for e in i)


class TestLeskFilterYes(unittest.TestCase):

  def setUp(self):
    self.filter = Yes(corpus2.get_named_tagset('nkjp'), {})

  def test_it(self):
    self.assertIn('abcd', self.filter)
    self.assertIn('fgdesrg', self.filter)
    self.assertIn('ab24232cd', self.filter)
    self.assertIn('8ki67j', self.filter)
    self.assertIn('rtert', self.filter)
    self.assertIn('232 4 345', self.filter)



