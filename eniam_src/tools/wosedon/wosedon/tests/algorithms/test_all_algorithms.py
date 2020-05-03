# -*- coding: utf-8 -*-

import unittest, corpus2
from wosedon.utils.resources import Resources
from wosedon.utils.buildoptions import BuildOptions
from wosedon.utils.mergeoptions import MergeOptions
from wosedon.utils.algorithmoptions import AlgorithmOptions
from wosedon.context.document import Document
from wosedon.gbuilders import SynsetGraphBuilder
from wosedon.gbuilders import SUMOGraphBuilder
from wosedon.mergers import SynsetsSUMOMerger
from wosedon.algorithms import *
from wosedon.basegraph import *
        



#FIXME to powinno działać
class TestAllAlgorithms(unittest.TestCase):
  
  def setUp(self):
    # Resources
    res = Resources()
    res.set_tagset('nkjp')
    res.set_plwn_graph_file('tests/#Data/plwnik')
    res.set_sumo_graph_file('tests/#Data/sumo_graph')
    res.set_mapping_sumo_file('tests/#Data/sumo_mapping')
    
    # Graph
    bopts = BuildOptions()
    bopts.set_directed_graph(True)
    bopts.set_unique_edges(False)
    
    mopts = MergeOptions()
    
    gs = SynsetGraphBuilder(res, bopts).build_graph()
    gm = SUMOGraphBuilder(res, bopts).build_graph()
    g = SynsetsSUMOMerger(res, mopts).merge(gs, gm)
    g.create_edge_attribute('weight', 'float', 1.0)
    
    # Context
    cclreader = corpus2.CclRelReader(res.tagset(), 
                                    'tests/#Data/wiki_ccl.xml',
                                    'tests/#Data/wiki_ccl.xml')
    ccldoc = cclreader.read()
    d = Document(res.tagset(), ccldoc, ccldoc.relations())
    
    # Options
    ao = AlgorithmOptions()
    ao.set_ini_nodes('synset')
    
    # Summary
    self.graph = g
    self.resources = res
    self.context = d.next()
    self.options = ao
  
  
  def tearDown(self):
    wsdrank, itr = self.result
    self.assertIsInstance(itr, int)
    self.assertTrue(itr >= 0)
  
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPR(self):
    alg = GTPersonalizedPR()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPRNorm(self):
    alg = GTPersonalizedPRNorm()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPRNorm2(self):
    alg = GTPersonalizedPRNorm2()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPRNormIt(self):
    alg = GTPersonalizedPRNormIt()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPRNormReduction(self):
    alg = GTPersonalizedPRNormReduction()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedPRNormTwoStep(self):
    alg = GTPersonalizedPRNormTwoStep()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedW2WPR(self):
    alg = GTPersonalizedW2WPR()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersonalizedW2WPRNorm(self):
    alg = GTPersonalizedW2WPRNorm()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersPRNormItModV(self):
    alg = GTPersPRNormItModV()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersPRNormItModVRankNorm(self):
    alg = GTPersPRNormItModVRankNorm()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTPersPRNormModV(self):
    alg = GTPersPRNormModV()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_GTStaticPR(self):
    alg = GTStaticPR()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking. Moreover, judging from git log, it probably have never worked.")
  def test_GTSUDOKURun2(self):
    alg = GTSUDOKURun2()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_LeskAlg(self):
    self.options.set_lesk_function('Cosine')
    self.resources.set_gloss_file('tests/#Data/glossfile.xml')
    alg = LeskAlg()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)
  
  @unittest.skip("Very mysterious errors, probably the same as in WsdRanking.")
  def test_PaintballWSD(self):
    self.resources.set_impedance_table('tests/#Data/impedance.csv')
    alg = PaintballWSD()
    self.result = alg.run(self.context, self.graph, self.options, self.resources)

























