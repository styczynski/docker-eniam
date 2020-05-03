#!/usr/bin/python
# -*- coding: utf-8 -*-

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

'''
WoSeDon - Word Sense Disambiguation tool for processing polish language.
WoSeDon based on the PageRank approach to make disambiguation, uses
many resources to build the graphs. These graphs can be merged into one
larger by many available mergers.
'''

import os, sys, time, logging, tempfile
import corpus2
import wosedon_base
from utils.config import Config
#from ranking.wsd_ranking import WSDRanking
from visualisation import Visualisation


def _make_parser(desc = ''):
  aparser = wosedon_base.make_parser(desc)
  meg = aparser.add_mutually_exclusive_group(required = True)
  meg.add_argument('-f', '--cclfile', dest = 'ccl',
                   type = str, help = 'Ccl file.')
  meg.add_argument('-i', '--interactive', dest = 'interactive',
                   action = 'store_true',
                   help = (
                     'If interactive mode is enabled, then instead of a filename '
                     'argument the user is expected to provide the Ccl-formatted '
                     'data to be parsed as WoSeDon\'s input. In this mode '
                     'processing is performed in a loop: the user can provide '
                     'several chunks of data, and they will be processed in '
                     'order. Each must however be terminated with an empty line.')),
  aparser.add_argument('-r', '--relcclfile', dest = 'relccl',
                       required = False, type = str, help = 'Relccl file')
  aparser.add_argument('-o', '--out-file', dest = 'out_file', default = '-',
                       help = (
                         'Output file. Default is set to write output '
                         'to stdout (\"-\") if set filename, then output will be ' 
                         'stored to given filename.'))
  aparser.add_argument('-b', '--batch', dest = 'batch',
                       action = 'store_true',
                       help = (
                         'If batch mode is enabled, then into -f file is '
                         'the index of files to be disambiguated. The index '
                         'contains the paths to the files: one path per one '
                         'line. New, disambiguated file has new name with '
                         'postfix like a *.wosedon.xml')),
  aparser.add_argument('-vd', '--visualisation-dir', dest = 'visualisation_dir',
                       required = False,
                       help = ('Path to directory for PageRank visualisation.'))
  return aparser


def _run_disambiguation(graph, context, wsd_alg, cfg, alfa=None, v_dir=None, 
                        output_filename=None, show_iterations=False,
                        model_result_list=None):
  
  logger = logging.getLogger(__name__)
  
  
  ret_iter_list = []
  # for given context in "c" run disambiguation method
  # to disambiguation use the built "graph" graph
  c = context.next()
  while c:
    (wsd_rank, ret_iter) = wsd_alg.run(
      c, graph, cfg.algorithm_options(), cfg.resources())
    ctx_rank = wsd_rank.get_ranking_for_context(c, graph)
    # run reranking for all rerankers
    for reranker in cfg.rerankers():
      logger.info('Reranking using %s.', str(reranker))
      ctx_rank = reranker.rerank(ctx_rank, graph, cfg.rerank_options())

    # add annotation
    if model_result_list != None:
      model_result_list.append(ctx_rank)
    else:
      wosedon_base.add_annotations_from_rank(ctx_rank, graph, alfa)

    if show_iterations:
      ret_iter_list.append(ret_iter)
    if v_dir:
      Visualisation().make_visualisation(c, graph, wsd_rank, v_dir)
    c =  context.next()
  # the context should be reseted at the end
  context.reset()
  
  if output_filename:
    # write the output filename
    wosedon_base.write_ccl_file(cfg, context.ccl_document(), output_filename)

  if show_iterations:
    for i, ret_iter in enumerate(ret_iter_list):
      looger.info("Total number of iterations (context %s):", str(i), str(ret_iter))


def main(argv = None):
  logger = logging.getLogger(__name__)
  
  p = _make_parser(__doc__)
  args = p.parse_args(argv)

  cfg = _read_config(args.config)
  builders, mergers, wsd_alg, g =_parse_cfg_object(cfg, args)

  if args.batch:
    with open(args.ccl, 'rt') as batch_file:
      for line in batch_file:
        start = time.clock()
        ccl_filename = line.strip()
        out_ccl_filename = ccl_filename + ".wosedon.xml"
        logger.info('Processing %s.', ccl_filename)

        # makes contex object based on the ccl file from ccl index file
        context = cfg.context(ccl_filename, ccl_filename)
        try:
          _run_disambiguation(
            g, 
            context, 
            wsd_alg, 
            cfg, 
            float(args.alpha) if args.alpha else None, 
            args.visualisation_dir,
            out_ccl_filename, 
            args.iterations)
          logger.info('Done in %.2f.', time.clock() - start)
        except Exception, e:
          print e
  elif args.interactive:
    tmp = tempfile.NamedTemporaryFile()
    l = sys.stdin.readline()
    while len(l) > 0:
      if len(l) > 1:
        tmp.write(l)
      else:
        tmp.seek(0)

        context = cfg.context(tmp.name, tmp.name)
        _run_disambiguation(
          g, 
          context, 
          wsd_alg, 
          cfg, 
          float(args.alpha) if args.alpha else None,
          args.visualisation_dir,
          args.out_file,
          args.iterations
        )
            
        tmp.close()
        tmp = tempfile.NamedTemporaryFile()

      l = sys.stdin.readline()

    tmp.close()  
  else:
    context = cfg.context(args.ccl, 
                          args.ccl if not args.relccl else args.relccl)
    _run_disambiguation(
      g, 
      context, 
      wsd_alg, 
      cfg, 
      float(args.alpha) if args.alpha else None,
      args.visualisation_dir,
      args.out_file,
      args.iterations
    )


def disambiguate_document(document, config, wsd_objects=None):
  """!
  Run WoSeDon disambiguation on the given document object.

  The input document is an document object of corpus2. The path to
  a configuration has to contain all sections required by WoSeDon.
  The output is a document (for future use, right now it is the same object as
  for the input) and a tuple of WSD objects that may be used for the next run
  of WSD (unless you want to use another configuration file

  @param document a document object to be disambiguated.
  @type document: corpus2.Document|corpus2.DocumentPtr
  @param config the path to a configuration file or a configuration object.
  @type config: str|wosedon.utils.config.Config
  @param wsd_objects WSD objects from a previous run. If not given they will
    be created from the scratch based on the provided configuration file.
  @type wsd_objects: tuple

  @return a disambiguated document object, a configuration object and
    WSD objects.
  @rtype: (corpus2.Document|corpus2.DocumentPtr, wosedon.utils.config.Config,
    tuple)
  """
  if isinstance(config, str):
    cfg = _read_config(config)
  else:
    cfg = config
  if wsd_objects:
    builders, mergers, wsd_alg, graph = wsd_objects
  else:
    builders, mergers, wsd_alg, graph = _parse_cfg_object(cfg)
  context = cfg.create_context(document)
  _run_disambiguation(
      graph, 
      context, 
      wsd_alg, 
      cfg
  )
  return document, cfg, (builders, mergers, wsd_alg, graph)

def _read_config(config_path):
  """! Load and return Config object. """
  return Config(config_path)

def _parse_cfg_object(cfg, args=None):
  """! Parse the config object to base objects and return them. """
  builders, mergers, wsd_alg, graphs = \
      wosedon_base.parse_cfg_to_base_objects(cfg, args)
  # FIXME: Uwaga! konfig moze zwrocic wiele grafow, ale aktualnie tylko
  # FIXME: jeden biore pod uwage w wykonywaniu algorytmu! pierwszy
  g = graphs[0]
  return builders, mergers, wsd_alg, g


if __name__ == '__main__':
  main()
