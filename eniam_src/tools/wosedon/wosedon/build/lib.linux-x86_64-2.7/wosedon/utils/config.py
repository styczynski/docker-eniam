#
# <one line to give the library's name and an idea of what it does.>
# Copyright (C) 2014  <copyright holder> <email>
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
#

import corpus2
from resources import Resources
from buildoptions import BuildOptions
from mergeoptions import MergeOptions
from algorithmoptions import AlgorithmOptions
from rerankoptions import RerankOptions

MODULE_GBUILDERS = 'wosedon.gbuilders'
MODULE_MERGERS = 'wosedon.mergers'
MODULE_CONTEXT = 'wosedon.context'
MODULE_ALGORITHMS = 'wosedon.algorithms'
MODULE_RERANKS = 'wosedon.reranks'

SEC_WSD = 'wosedon'
WSD_OPT_GBUILDERS = 'gbuilders'
WSD_OPT_CONTEXT = 'context'
WSD_OPT_MERGERS = 'mergers'
WSD_OPT_WSD_ALG = 'wsdalgorithm'
WSD_OPT_RERANKERS = 'rerankers'
WSD_OPT_USE_WEIGHTS = 'use_weights'
WSD_OPT_WEIGHTS = 'weights'

SEC_WSD_RES = 'wosedon:resources'
WSD_RES_OPT_SUMO_GRAPH = 'sumo_graph_file'
WSD_RES_OPT_CCL_GRAPH = 'ccl_graph_file'
WSD_RES_OPT_SUMO_MAPPING = 'mapping_sumo_file'
WSD_RES_OPT_WND_GRAPH = 'wnd_graph_file'
WSD_RES_OPT_WND_MAPPING = 'mapping_wnd_file'
WSD_RES_OPT_MSR = 'msr_file'
WSD_RES_OPT_PLWN_GRAPH = 'plwn_graph_file'
WSD_RES_OPT_GLOSS_FILE = 'gloss_file'
WSD_RES_OPT_GLOSS_REL_FILE = 'gloss_rel_file'
WSD_RES_OPT_TAGSET = 'tagset'
WSD_RES_OPT_IMPEDANCE_FILE = 'impedance_table'

SEC_WSD_BUILD = 'wosedon:build_options'
WSD_BUILD_OPT_ACC_LEXICON = 'accept_lexicon'
WSD_BUILD_OPT_UNIQ_EDGES = 'unique_edges'
WSD_BUILD_OPT_DI_GRAPH = 'directed_graphs'
WSD_BUILD_OPT_SYN_RELS_IDS = 'syn_rel_ids'
WSD_BUILD_OPT_LU_RELS_IDS = 'lu_rel_ids'
WSD_BUILD_OPT_ACC_POS = 'accept_pos'
WSD_BUILD_OPT_ADD_REVERSED_EDGES = 'add_reversed_edges'

SEC_WSD_MERGE = 'wosedon:merge_options'

SEC_WSD_ALG = 'wosedon:wsd_alg'
WSD_ALG_DAMP_FACTOR = 'damping_factor'
WSD_ALG_MAX_ITER = 'max_iter'
WSD_ALG_INI_NODES = 'ini_nodes'
WSD_ALG_LESK_FUNC = 'lesk_function'
WSD_ALG_LESK_FILT = 'lesk_filter'

SEC_WSD_RERANK = 'wosedon:rerank_options'
WSD_RERANK_PERCENTAGE_DIFF = 'percentage_diff'

SEC_WSD_LESK_FILTER = 'wosedon:lesk_filter'
WSD_LF_OPT_INPUT_FILE = 'list_file'
WSD_LF_OPT_ALLOW_ONLY = 'allow_only'


class Config:
  def __init__(self, cfg_file_path=''):
    from ConfigParser import ConfigParser
    self._config = ConfigParser()
    
    # list of graph builders
    self._gbuilders = []
    # list of mergers
    self._mergers = []
    # algorithm which will be used into wsd process
    self._wsd_algorithm = None
    # list of rerankers used after running the algorithm
    self._rerankers = []

    # Context type
    self._context = None
    # Should weights be used
    self._use_weights = False
    # Weights
    self._weights = {}

    # Resources object
    self._resources = None
    # BuildOptions object
    self._build_options = None
    # MergeOptions object
    self._merge_options = None
    # AlgorithmOptions object
    self._algorithm_options = None
    # RerankOptions object
    self._rerank_options = None

    self._cfg_file_path = cfg_file_path
    if cfg_file_path:
      self.parse(cfg_file_path)

  def gbuilders(self):
    gb_list = []
    for gb in self._gbuilders:
      module = __import__(MODULE_GBUILDERS, fromlist=[gb])
      gb_list.append(getattr(module, gb)(self.resources(), self.build_options()))
    return gb_list

  def mergers(self):
    mrg_list = []
    for mrg in self._mergers:
      module = __import__(MODULE_MERGERS, fromlist=[mrg])
      mrg_list.append(getattr(module, mrg)(self.resources(), self.merge_options()))
    return mrg_list
  
  def wsd_algorithm(self):
    if isinstance(self._wsd_algorithm, str):
      module = __import__(MODULE_ALGORITHMS, fromlist=[self._wsd_algorithm])
      self._wsd_algorithm = getattr(module, self._wsd_algorithm)()
    return self._wsd_algorithm
  
  def rerankers(self):
    if isinstance(self._rerankers, str):
      reranks = []
      for rerank in self._rerankers.split():
        module = __import__(MODULE_RERANKS, fromlist=[rerank])
        reranks.append(getattr(module, rerank)())
      self._rerankers = reranks
    return self._rerankers
  
  def context(self, ccl_file, ccl_rel):
    if self._context:
      cclreader = corpus2.CclRelReader(self.resources().tagset(), 
                                       ccl_file, ccl_rel)
      ccldoc = cclreader.read()
      module = __import__(MODULE_CONTEXT, fromlist=[self._context])
      return getattr(module, self._context)(self.resources().tagset(), ccldoc, ccldoc.relations())
    return None

  def create_context(self, document):
    if self._context:
      module = __import__(MODULE_CONTEXT, fromlist=[self._context])
      return getattr(module, self._context)(
                                            self.resources().tagset(), 
                                            document, 
                                            document.relations())
    else:
      return None
  
  def weights(self):
    if self._use_weights:
      return self._weights
    return None

  def resources(self):
    return self._resources
  
  def build_options(self):
    return self._build_options

  def merge_options(self):
    return self._merge_options
  
  def algorithm_options(self):
    return self._algorithm_options

  def rerank_options(self):
    return self._rerank_options

  def parse(self, cfg_file_path):
    self._cfg_file_path = cfg_file_path
    self._resources = Resources()
    self._build_options = BuildOptions()
    self._merge_options = MergeOptions()
    self._algorithm_options = AlgorithmOptions()
    self._rerank_options = RerankOptions()
    
    with open(self._cfg_file_path, 'r') as ini_file:
      self._config.readfp(ini_file)

      # Obligatory option
      # gbuilders / makes string-like representation only...
      self._gbuilders = \
          self._config.get(SEC_WSD, WSD_OPT_GBUILDERS).split()

      # Obligatory option
      # A string like representation of a context type
      self._context = self._config.get(SEC_WSD, WSD_OPT_CONTEXT)

      # Mergers are optional, but the option is in an obligatory section.
      if self._config.has_option(SEC_WSD, WSD_OPT_MERGERS):
        # mergers / makes string-like representation only...
        self._mergers = \
            self._config.get(SEC_WSD, WSD_OPT_MERGERS).split()

      # Obligatory option
      # disambiguation method
      self._wsd_algorithm = self._config.get(SEC_WSD, WSD_OPT_WSD_ALG)

      # Rerankers are optional, but the option is in an obligatory section.
      if self._config.has_option(SEC_WSD, WSD_OPT_RERANKERS):
        # The split and class initializations are in method rerankers
        self._rerankers = self._config.get(SEC_WSD, WSD_OPT_RERANKERS)

      # Weights
      if self._config.has_option(SEC_WSD, WSD_OPT_USE_WEIGHTS):
        self._use_weights = self._config.getboolean(SEC_WSD, WSD_OPT_USE_WEIGHTS)
      if self._config.has_option(SEC_WSD, WSD_OPT_WEIGHTS):
        self._weights = self._build_dict(
                SEC_WSD,
                WSD_OPT_WEIGHTS,
                key_type = str,
                val_type = float
            )

      # resources
      self._load_resources()

      # build options
      self._load_build_options()

      # merge options
      self._load_merge_options()
      
      # algorithm options
      self._load_algorithm_options()

      # rerank options
      self._load_rerank_options()

  def _load_resources(self):
    """ Read and load all available resources from the config. """
    # This option is obligatory, ConfigParser will raise an exception if it
    # is missing.
    self._resources.set_plwn_graph_file(
        self._config.get(SEC_WSD_RES, WSD_RES_OPT_PLWN_GRAPH)
    )
    # This option is obligatory, ConfigParser will raise an exception if it
    # is missing.
    self._resources.set_tagset(self._config.get(SEC_WSD_RES, WSD_RES_OPT_TAGSET))
    
    # This section is obligatory, thus no need to check whether it exists
    # or not. An exception should be raised either way.
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_SUMO_GRAPH):
      self._resources.set_sumo_graph_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_SUMO_GRAPH)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_CCL_GRAPH):
      self._resources.set_ccl_graph_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_CCL_GRAPH)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_SUMO_MAPPING):
      self._resources.set_mapping_sumo_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_SUMO_MAPPING)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_WND_GRAPH):
      self._resources.set_wnd_graph_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_WND_GRAPH)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_WND_MAPPING):
      self._resources.set_mapping_wnd_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_WND_MAPPING)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_MSR):
      self._resources.set_msr_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_MSR)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_GLOSS_FILE):
      self._resources.set_gloss_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_GLOSS_FILE)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_GLOSS_REL_FILE):
      self._resources.set_gloss_rel_file(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_GLOSS_REL_FILE)
      )
    if self._config.has_option(SEC_WSD_RES, WSD_RES_OPT_IMPEDANCE_FILE):
      self._resources.set_impedance_table(
          self._config.get(SEC_WSD_RES, WSD_RES_OPT_IMPEDANCE_FILE)
      )

  def _load_build_options(self):
    """ Read and load all available build options from the config. """
    if self._config.has_section(SEC_WSD_BUILD):
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_ACC_LEXICON):
        self._build_options.set_accept_lexicon(
            [str(lexicon) for lexicon in
                self._config.get(
                    SEC_WSD_BUILD,
                    WSD_BUILD_OPT_ACC_LEXICON).split()]
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_UNIQ_EDGES):
        self._build_options.set_unique_edges(
            self._config.getboolean(SEC_WSD_BUILD, WSD_BUILD_OPT_UNIQ_EDGES)
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_DI_GRAPH):
        self._build_options.set_directed_graph(
            self._config.getboolean(SEC_WSD_BUILD, WSD_BUILD_OPT_DI_GRAPH)
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_SYN_RELS_IDS):
        self._build_options.set_syn_rel_ids(
            [int(ids) for ids in
                self._config.get(
                    SEC_WSD_BUILD,
                    WSD_BUILD_OPT_SYN_RELS_IDS).split()]
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_LU_RELS_IDS):
        self._build_options.set_lu_rel_ids(
            [int(ids) for ids in
                self._config.get(
                    SEC_WSD_BUILD,
                    WSD_BUILD_OPT_LU_RELS_IDS).split()]
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_ACC_POS):
        self._build_options.set_accept_pos(
            [int(pos) for pos in
                self._config.get(
                    SEC_WSD_BUILD,
                    WSD_BUILD_OPT_ACC_POS).split()]
        )
      if self._config.has_option(SEC_WSD_BUILD, WSD_BUILD_OPT_ADD_REVERSED_EDGES):
        self._build_options.set_add_reversed_edges(
            self._build_dict(
                SEC_WSD_BUILD,
                WSD_BUILD_OPT_ADD_REVERSED_EDGES,
                key_type=int,
                val_type=int
            )
        )

  def _build_dict(self, section, option, key_type=str, val_type=str):
    """ Convert a dictionary string into a dictionary object.

    A dictionary string looks as follows:
      key1:val1 key2:val2 key3:val3 ...

    @param section the name of a section in the config file.
    @type section: str
    @param option the name of a section's option in the configure file.
    @type option: str
    @param key_type a function for key convertion, a key is a string.
    @type key_type: function
    @param val_type a function for value convertion, a value is a string.
    @type val_type: function

    @return a dictionary object
    @rtype: dict
    """
    raw_weights = self._config.get(section, option).split()
    weight_dict = {}
    for id_weight in raw_weights:
      id_, weight = id_weight.split(':')
      weight_dict[key_type(id_)] = val_type(weight)
    return weight_dict

  def _load_merge_options(self):
    """ Read and load all available merge options from the config. """
    if self._config.has_section(SEC_WSD_MERGE):
      pass

  def _load_algorithm_options(self):
    """ Read and load all available algorithm options from the config. """
    # This section is obligatory, thus no need to check whether it exists
    # or not. An exception should be raised either way.
    if self._config.has_option(SEC_WSD_ALG, WSD_ALG_DAMP_FACTOR):
      self._algorithm_options.set_damping_factor(
          self._config.get(SEC_WSD_ALG, WSD_ALG_DAMP_FACTOR)
      )
    if self._config.has_option(SEC_WSD_ALG, WSD_ALG_MAX_ITER):
      self._algorithm_options.set_max_iter(
          self._config.getint(SEC_WSD_ALG, WSD_ALG_MAX_ITER)
      )
    if self._config.has_option(SEC_WSD_ALG, WSD_ALG_INI_NODES):
      self._algorithm_options.set_ini_nodes(
        [ini_node for ini_node in
          self._config.get(
            SEC_WSD_ALG, 
            WSD_ALG_INI_NODES).split()]
      )      
    if self._config.has_option(SEC_WSD_ALG, WSD_ALG_LESK_FUNC):
      self._algorithm_options.set_lesk_function(
          self._config.get(SEC_WSD_ALG, WSD_ALG_LESK_FUNC)
      )
    if self._config.has_option(SEC_WSD_ALG, WSD_ALG_LESK_FILT):
      self._algorithm_options.set_lesk_filter(
          self._config.get(SEC_WSD_ALG, WSD_ALG_LESK_FILT)
      )
    
    if self._config.has_section(SEC_WSD_LESK_FILTER):
      lfo = self._algorithm_options.lesk_filter_opts()
      
      if self._config.has_option(SEC_WSD_LESK_FILTER, WSD_LF_OPT_INPUT_FILE):
        lfo.set_input_file(self._config.get(SEC_WSD_LESK_FILTER, WSD_LF_OPT_INPUT_FILE))
      
      if self._config.has_option(SEC_WSD_LESK_FILTER, WSD_LF_OPT_ALLOW_ONLY):
        lfo.set_allow_only(self._config.getboolean(SEC_WSD_LESK_FILTER, WSD_LF_OPT_ALLOW_ONLY))
    

  def _load_rerank_options(self):
    """ Read and load all available rerankers options from the config. """
    if self._config.has_option(SEC_WSD_RERANK, WSD_RERANK_PERCENTAGE_DIFF):
      self._rerank_options.set_percentage_diff(
          self._config.getint(SEC_WSD_RERANK, WSD_RERANK_PERCENTAGE_DIFF)
      )
    # This option is obligatory, ConfigParser will raise an exception if it
    # is missing.
    self._rerank_options.set_tagset(self._resources.tagset())
