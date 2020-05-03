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


# This package contains base functions which can are used into wosedon 
# applications. For example base parser for config, model dir and total 
# number of iterations. There is a general method to convert a whole 
# rank to string representation, to adds annotations to CCL files and 
# returns its. An output of annotated CCLs may be returned to stdout or 
# to given filename.

import sys, os, logging
import corpus2
import argparse
import operator

import domains as dom

from basegraph import BaseGraph
try:
    import argcomplete
except ImportError:
    argcomplete = None
    
def make_parser(desc = ''):
  aparser = argparse.ArgumentParser(description = desc)

  aparser.add_argument('-c', '--config-file', dest='config',
                       default='cfg/wosedon.ini',
                       help='WoSeDon config file.')
  aparser.add_argument('-md', '--model-dir', dest='model_dir', 
                       required=False, help=(
                         'Output directory where the models will be stored. '
                         'If is not given then the models will not be stored. '
                         'If output directory is given, and the models '
                         'exists into directory, then tey will be loaded.'))
  aparser.add_argument('-it', '--iterations', dest='iterations', 
                       action='store_true', help=(
                         'Total number of algorithm iterations will be shown '
                         'after WSD algorithm running.'))
  aparser.add_argument('-a', '--alpha', dest='alpha', required=False,
                       help=(
                         'If option is given, then the ranking is '
                         'returned only for these lemmas, where the ranking '
                         'on the first position is a much larger than on '
                         'the second position. The diffrence between first '
                         'and second position is sotred as value of option.'))
  aparser.add_argument('-V', '--verbose', dest='verbose', 
                       action='count',
                       help='If -V is set then warnings will be shown. If it be -VV '
                            'also there will be shown standard informations. Be it '
                            '-VVV even debug output will be shown.')
  if argcomplete:
    argcomplete.autocomplete(aparser)
  return aparser

def make_whole_rank_str(rank):
  '''Converts whole ranking to string like format'''
  return ' '.join('%d/%.10f' % (r[0], r[1]) for r in rank)

def make_unitstr(graph, node):
  fst_unitsstr = ''
  for lu in node.synset.lu_set:
    lu_lemma = str(lu.lemma).replace(' ', '_')
    lu_variant = int(lu.variant)
    lu_domain = int(lu.domain)
    lu_domain_str = dom.DOMAINS[lu_domain] \
      if dom.DOMAINS.has_key(lu_domain) \
      else str(lu_domain)
    manual_synset_str = ('%s.%d(%d:%s) ' % \
                        (lu_lemma, 
                         lu_variant, 
                         lu_domain, 
                         lu_domain_str))
    fst_unitsstr += manual_synset_str
  return fst_unitsstr

def add_annotations_from_rank(ranking, graph, alpha = None):
  for (token, node_rank) in ranking:
    if not node_rank:
      continue

    md_ptr = token.get_metadata()
    if not md_ptr:
      token.create_metadata()
      md_ptr = token.get_metadata()
    
    synset_rank = [(r[0].synset.synset_id, r[1]) 
                   for r in node_rank]

    if len(synset_rank) > 1:
      if alpha:
        diff = synset_rank[0][1] - synset_rank[1][1]
        if (synset_rank[0][1] - synset_rank[1][1]) <= alpha:
          continue

    # best synset ID
    fst_rank_synid = str(synset_rank[0][0])
    # best unit string
    fst_unitsstr = make_unitstr(graph, node_rank[0][0])
    whole_rank_norm_str = make_whole_rank_str(synset_rank)
    md_ptr.set_attribute('sense:ukb:syns_id', fst_rank_synid)
    md_ptr.set_attribute('sense:ukb:syns_rank', whole_rank_norm_str)
    md_ptr.set_attribute('sense:ukb:unitsstr', fst_unitsstr[:-1])

def write_ccl_file(cfg, ccl_document, outccl):
  writer = (
    corpus2.TokenWriter.create_stdout_writer('ccl', cfg.resources().tagset())
      if outccl[0] == '-' \
      else \
        corpus2.TokenWriter.create_path_writer(\
          'ccl', outccl, cfg.resources().tagset()))
  for paragraph in ccl_document.paragraphs():
    writer.write_chunk(paragraph)


def prepare_logging(args):
  loglevel = logging.ERROR
  if args.verbose >= 3:
    loglevel = logging.DEBUG
  elif args.verbose >= 2:
    loglevel = logging.INFO
  elif args.verbose >= 1:
    loglevel = logging.WARNING
  
  class InfoFilter(logging.Filter):
    def filter(self, rec):
      return rec.levelno <= logging.INFO
  
  format_ = "[%(asctime)s]\t%(name)s:%(levelname)s\t%(message)s"
  date_format = "%d %b, %H:%M"
  
  logger = logging.getLogger()
  logger.setLevel(loglevel)
  
  h1 = logging.StreamHandler(sys.stdout)
  h1.setLevel(logging.NOTSET)
  h1.addFilter(InfoFilter())
  
  h2 = logging.StreamHandler(sys.stderr)
  h2.setLevel(logging.WARNING)
  h2.setFormatter(logging.Formatter(format_, date_format))

  logger.addHandler(h1)
  logger.addHandler(h2)


def parse_cfg_to_base_objects(cfg, args=None):
  '''Returns list of objects based on the given config file 
  and application arguments. Parse its and returns the vector with:
  [
   builders,
   mergers,
   wsd_alg,
   list of graphs
  ]
  '''
  prepare_logging(args)
  
  logger = logging.getLogger(__name__)
  
  builders = cfg.gbuilders()
  mergers = cfg.mergers()
  wsd_alg = cfg.wsd_algorithm()

  if len(builders) < 1:
    logger.error('There is not set any builder so graph build is not possible. Have a nice day!')
    exit(1)

  if args and args.model_dir:
    if not os.path.exists(args.model_dir):
      os.makedirs(args.model_dir)

  # makes graphs -> decision about reading/writing from models etc.
  graphs = []
  if len(mergers) > 0:
    if args and args.model_dir:
      gb_filename = "merged_graph.xml.gz"
      gb_file_path = os.path.join(args.model_dir, gb_filename)
      if os.path.exists(gb_file_path):
        logger.info('Loading graph from file %s.', gb_file_path)
        g = BaseGraph()
        g.unpickle(gb_file_path)
      else:
        logger.info('Building graph using mergers.')
        g = builders[0].build_graph()
        for gnum in range(1, len(builders)):
          for mrg in mergers:
            g = mrg.merge(g, builders[gnum].build_graph())
        logger.info('Saving graph to file %s.', gb_file_path)
        g.pickle(gb_file_path)
    else:
      logger.info('Building graph using mergers.')
      g = builders[0].build_graph()
      for gnum in range(1, len(builders)):
        for mrg in mergers:
          g = mrg.merge(g, builders[gnum].build_graph())
    graphs = [g]
  else:
    for gb in builders:
      if args and args.model_dir:
        gb_filename = str(gb) + ".xml.gz"
        gb_file_path = os.path.join(args.model_dir, gb_filename)
        if os.path.exists(gb_file_path):
          logger.info('Loading graph for %s.', str(gb))
          g = BaseGraph()
          g.unpickle(gb_file_path)
        else:
          logger.info('Building graph using %s.', str(gb))
          g = gb.build_graph()
          logger.info('Saving graph to file %s', gb_file_path)
          g.pickle(gb_file_path)
      else:
        logger.info('Building graph using %s.', str(gb))
        g = gb.build_graph()
      graphs.append(g)
      break  # TODO while program cannot use more than one graph there is no need
             # to build them. When status quo changes this 'break' should be removed.
  
  
  logger.info('Setting weights.')
  warned = set()  # lest warn twice
  weights = cfg.weights()
  for g in graphs:
    g.create_edge_attribute('weight', "float", 0.0)
    if weights:
      for e in g.all_edges():
        if e.rel in weights:
          e.weight = weights[e.rel]
        elif e.rel != '' and e.rel not in warned:
          logger.warning('Warning: there is no weight for "%s"! Assuming 0.', e.rel)
          warned.add(e.rel)
    else:
      for e in g.all_edges():
        e.weight = 1.0
  
  logger.info('Building complete.')
  
  return [builders, mergers, wsd_alg, graphs]
