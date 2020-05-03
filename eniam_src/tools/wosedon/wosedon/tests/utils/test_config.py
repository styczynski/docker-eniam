# -*- coding: utf-8 -*-
import unittest
from wosedon.utils.config import Config

from wosedon.gbuilders.graphbuilderinterface import GraphBuilderInterface
from wosedon.mergers.mergerinterface import MergerInterface
from wosedon.algorithms.wsdalgorithminterface import WSDAlgorithmInterface
from wosedon.reranks.rerankinterface import RerankInterface

from wosedon.gbuilders import SynsetGraphBuilder, SUMOGraphBuilder, LexicalUnitGraphBuilder
from wosedon.mergers import SynsetsSUMOMerger, SynsetsLUMerger2
from wosedon.algorithms import LeskAlg, GTStaticPR, GTPersonalizedPRNorm2
from wosedon.reranks import LemmaRankingNormalizer, LemmaRankingSelecter




class TestConfig(unittest.TestCase):
  
  def setUp(self):
    self.cfg = Config()
  
  
  def test_buid_dict(self):
    self.cfg.parse('tests/#Data/wosedon_dicts.ini')
    a = self.cfg._build_dict('aaa', 'aa', str, int)
    b = self.cfg._build_dict('bbb', 'bb', int, float)
    c = self.cfg._build_dict('ccc', 'cc', str, str)
    
    self.assertEquals(a, {'a':1, 'b':2, 'c':3})
    self.assertEquals(b, {1:-1.1, 2:2.2, -3:-3.3})
    self.assertEquals(c, {'aaa':'bbb', 'zażółć':'gęślą', 'jaźń':'ccc'})
  
  
  def test_weights1(self):
    self.cfg.parse('tests/#Data/wosedon.ini')
    self.assertEquals(self.cfg.weights(), None)
  
  
  def test_weights2(self):
    self.cfg.parse('tests/#Data/wosedon_lu.ini')
    self.assertIsInstance(self.cfg.weights(), dict)
    self.assertEquals(len(self.cfg.weights()), 175)
  
  
  def test_weights3(self):
    self.cfg.parse('tests/#Data/wosedon_sumo.ini')
    self.assertEquals(self.cfg.weights(), {})
  
  
  def assert_types(self):
    for i in self.cfg.gbuilders():
      self.assertIsInstance(i, GraphBuilderInterface)
      
    for i in self.cfg.mergers():
      self.assertIsInstance(i, MergerInterface)
      
    for i in self.cfg.rerankers():
      self.assertIsInstance(i, RerankInterface)
      
    self.assertIsInstance(self.cfg.wsd_algorithm(), WSDAlgorithmInterface)
  
  
  def test_file1(self):
    self.cfg.parse('tests/#Data/wosedon.ini')
    self.assert_types()
    self.assertEquals(len(self.cfg.gbuilders()), 1)
    self.assertEquals(len(self.cfg.mergers()), 0)
    self.assertEquals(len(self.cfg.rerankers()), 0)
    
    self.assertIsInstance(self.cfg.gbuilders()[0], SynsetGraphBuilder)
    self.assertIsInstance(self.cfg.wsd_algorithm(), LeskAlg)
    
    lfo = self.cfg.algorithm_options().lesk_filter_opts()
    self.assertEquals(lfo.allow_only(), False)
    self.assertEquals(lfo.input_file(), '/home/dekakaruk/Źródła/wosedon/wosedon/poses')
  
  
  def test_file2(self):
    self.cfg.parse('tests/#Data/wosedon_lu.ini')
    self.assert_types()
    self.assertEquals(len(self.cfg.gbuilders()), 2)
    self.assertEquals(len(self.cfg.mergers()), 1)
    self.assertEquals(len(self.cfg.rerankers()), 2)
    
    self.assertIsInstance(self.cfg.gbuilders()[0], SynsetGraphBuilder)
    self.assertIsInstance(self.cfg.gbuilders()[1], LexicalUnitGraphBuilder)
    self.assertIsInstance(self.cfg.mergers()[0], SynsetsLUMerger2)
    self.assertIsInstance(self.cfg.rerankers()[0], LemmaRankingNormalizer)
    self.assertIsInstance(self.cfg.rerankers()[1], LemmaRankingSelecter)
    self.assertIsInstance(self.cfg.wsd_algorithm(), GTStaticPR)
    
    lfo = self.cfg.algorithm_options().lesk_filter_opts()
    self.assertEquals(lfo.allow_only(), False)
    self.assertEquals(lfo.input_file(), None)
  
  
  def test_file3(self):
    self.cfg.parse('tests/#Data/wosedon_sumo.ini')
    self.assert_types()
    self.assertEquals(len(self.cfg.gbuilders()), 2)
    self.assertEquals(len(self.cfg.mergers()), 1)
    self.assertEquals(len(self.cfg.rerankers()), 0)
    
    self.assertIsInstance(self.cfg.gbuilders()[0], SynsetGraphBuilder)
    self.assertIsInstance(self.cfg.gbuilders()[1], SUMOGraphBuilder)
    self.assertIsInstance(self.cfg.mergers()[0], SynsetsSUMOMerger)
    self.assertIsInstance(self.cfg.wsd_algorithm(), GTPersonalizedPRNorm2)
    
    lfo = self.cfg.algorithm_options().lesk_filter_opts()
    self.assertEquals(lfo.allow_only(), True)
    self.assertEquals(lfo.input_file(), '/home/dekakaruk/Źródła/wosedon/wosedon/poses')
    












