#encoding: utf8
#
# <one line to give the library's name and an idea of what it does.>
# Copyright (C) 2015  <copyright holder> <email>
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
'''
WoSeDon plugin. It can be used into our web services. 

Sample usage:

# ------ CUT HERE
#encoding: utf8
from wosedon import wosedon_plugin 

def madafaka():
  cfg = '/home/pkedzia/repos/wosedon/wosedon/cfg/wosedon_official.ini'
  mdir = '/home/pkedzia/repos/wosedon/wosedon/resources/plug_model_dir'
  wsdlg = wosedon_plugin.WoSeDonPlugin(cfg, mdir)

  fpaths = [
    '/home/pkedzia/test-wosedon/in_1.ccl',
    '/home/pkedzia/test-wosedon/in_2.ccl',
    '/home/pkedzia/test-wosedon/in_3.ccl']

  for inccl in fpaths:
    outccl = inccl + ".wosedon.ccl"
    wsdlg.run_wosedon(inccl, outccl)
# ------ CUT HERE
'''

import logging, sys
import run_wosedon

class WoSeDonPlugin(object):
  def __init__(self, cfg_file_path, model_dir, verbose=True, argv=[]):
    """!
    Constructor makes the model of WoSeDon.

    Reads all required resources, build (or read) model and makes 
    the required objects. Uses the run_wosedon._make_parser method to
    parse available wosedon options.

    @param cfg_file_path path to wosedon config file
    @type cfg_file_path: String
    @param model_dir Path to directory when the built model will be stored.
    @type model_dir: String or None
    @param verbose Is set to true then plugin verbose mode is activated
    @type verbose: Boolean default True
    @param argv additional arguments
    @type argv: list default empty
    """
    self._verbose = verbose
    if verbose:
      logging.debug("Initializing WoSeDon plugin...")

    if verbose:
      logging.debug("Parsing WoSeDon arguments...")
    margv = argv
    margv.extend(['-f', "-"])
    margv.extend(['-c', cfg_file_path])
    margv.extend(['-md', model_dir] if model_dir else [])
    margv.extend(['-V'] if verbose else [])
    p = run_wosedon._make_parser(__doc__)
    self._args = p.parse_args(margv)

    if verbose:
      logging.debug("Reading WoSeDon config file...")
    self._wosedon_cfg = run_wosedon._read_config(self._args.config)

    if verbose:
      logging.debug("Reading WoSeDon models and resources...")
    self._builders, self._mergers, self._wsd_alg, self._g = \
      run_wosedon._parse_cfg_object(self._wosedon_cfg, self._args)

    if verbose:
      logging.debug("WoSeDon was initialized!")

    self._context = None

  @property
  def builders(self):
    """! Getter for builders """
    return self._builders

  @property
  def mergers(self):
    """! Getter for mergers """
    return self._mergers

  @property
  def algorithm(self):
    """! Getter for algorithm"""
    return self._wsd_alg

  @property
  def graph(self):
    """! Getter for graph """
    return self._g
  
  @property
  def config(self):
    """! Getter for read wosedon config """
    return self._wosedon_cfg

  def run_wosedon(self, in_cclfilepath, out_cclfilepath, options=None, 
                  model_result_list=None):
    """!
    Run disambiguation for given intput ccl file path.
    
    Store the result into output file in CCL format.
    
    @param in_cclfilepath
    @type in_cclfilepath: 
    @param out_cclfilepath
    @type out_cclfilepath:
    @param options
    @type options:
    """
    if self._verbose:
      logging.debug('Running WoSeDoN for file: %s', 
                    in_cclfilepath.decode('utf8'))

    self._context = self._wosedon_cfg.context(in_cclfilepath, in_cclfilepath)
    run_wosedon._run_disambiguation(
            self._g, self._context, self._wsd_alg, self._wosedon_cfg, 
            None, self._verbose, None, out_cclfilepath, None, 
            model_result_list)