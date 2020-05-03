import unittest
from wosedon.utils.algorithmoptions import AlgorithmOptions

from wosedon.utils.resources import Resources
from wosedon.basegraph import BaseGraph
from wosedon.lesk_functions.leskfunctioninterface import LeskFunctionInterface
from wosedon.lesk_filters.leskfilterinterface import LeskFilterInterface

from wosedon.lesk_functions import *
from wosedon.lesk_filters import *


class TestAlgorithmOptions(unittest.TestCase):
  
  def setUp(self):
    self.opts = AlgorithmOptions()
    self.res = Resources()
    self.res.set_tagset('nkjp')
    self.g = BaseGraph()
  
  
  def test_type(self):
    self.opts.set_lesk_function('Cosine')
    self.assertIsInstance(self.opts.lesk_function(self.res, self.g), LeskFunctionInterface)
    self.opts.set_lesk_function('ExampleFunction')
    self.assertIsInstance(self.opts.lesk_function(self.res, self.g), LeskFunctionInterface)
    self.opts.set_lesk_function('Intersection')
    self.assertIsInstance(self.opts.lesk_function(self.res, self.g), LeskFunctionInterface)
    
    self.opts.set_lesk_filter('POS')
    self.assertIsInstance(self.opts.lesk_filter(None), LeskFilterInterface)
    self.opts.set_lesk_filter('Yes')
    self.assertIsInstance(self.opts.lesk_filter(None), LeskFilterInterface)
    self.opts.set_lesk_filter('WordsPOS')
    self.assertIsInstance(self.opts.lesk_filter(None), LeskFilterInterface)
  
  
  def test_default(self):
    self.assertIsInstance(self.opts.lesk_filter(None), Yes)

