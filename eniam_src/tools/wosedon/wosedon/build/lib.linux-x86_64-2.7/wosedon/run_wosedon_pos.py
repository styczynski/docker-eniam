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

'''
PoSeDoN accept different models for different part of speech. 
It is possible to disambiguate the different POS using different models 
and also graphs.
'''

import sys
import corpus2
import run_wosedon
import wosedon_base
import wosedon_plugin
from context import context as ccc

try:
    import argcomplete
except ImportError:
    argcomplete = None

def make_parser(desc):
  """!
  Makes the parser for comman line options.
  @param desc An application descriptions
  @type desc: String
  @return Argument parser
  @rtype: argparse.ArgumentParser
  """
  aparser = wosedon_base.make_parser(desc)
  aparser.add_argument('-f', '--cclfile', dest='ccl',
                       required=True, type=str, help='Ccl file.')
  aparser.add_argument('-r', '--relcclfile', dest='relccl',
                       required=False, type=str, help='Relccl file')
  aparser.add_argument('-o', '--out-file', dest='out_file', default='-',
                       help=(
                         'Output file. Default is set to write output '
                         'to stdout (\"-\") if set filename, then output will be ' 
                         'stored to given filename.'))
  aparser.add_argument('-b', '--batch', dest='batch',
                       action='store_true', help=(
                         'If batch mode is enabled, then into -f file is '
                         'the index of files to be disambiguated. The index '
                         'contains the paths to the files: one path per one '
                         'line. New, disambiguated file has new name with '
                         'postfix like a *.wosedon.xml')),

  aparser.add_argument('-Other', '--other-pos-cfg', dest='other_pos_cfg', 
                       required=True, help=(
                         'This config is used when the specific POS has not '
                         'assigned config. This option is required.'))
  aparser.add_argument('-Noun', '--noun-pos-cfg', dest='noun_pos_cfg', 
                       required=False, help=(
                         'This config is used while the Noun words will be '
                         'disambiguated. When is not assigned, '
                         'then the config from option -O will be used.'))
  aparser.add_argument('-Verb', '--verb-pos-cfg', dest='verb_pos_cfg', 
                       required=False, help=(
                         'This config is used while the Verb words will be '
                         'disambiguated. When is not assigned, '
                         'then the config from option -O will be used.'))

  aparser.add_argument('-vd', '--visualisation-dir', dest='visualisation_dir',
                       required=False, help=(
                         'Path to directory for PageRank visualisation.'))
  return aparser

def load_models(oconfig, nconfig, vconfig, mdir, add_args, verbose):
  """!
  For the specific config, loads the appropriate models
  
  @param oconfig Config path to other POS
  @type oconfig: String
  @param nconfig Config path to noun POS
  @type nconfig: String
  @param vconfig Config path to verb POS
  @type vconfig: String
  @param add_args Additional options (derived from wosedon_base)
  @type: list
  @param verbose If set to True then PoSeDon is more verbose
  @verbose type: Boolean
  @return dictionary where as the key is used POS and as value the model
  @rtype: dict[pos_int]=>WoSeDonPlugin
  """
  mdl_dict = {}
  mdl_dict['o'] = wosedon_plugin.WoSeDonPlugin(
    oconfig, mdir, verbose=verbose, argv=add_args)

  if nconfig:
    mdl_dict['n'] = wosedon_plugin.WoSeDonPlugin(
      nconfig, mdir, verbose=verbose, argv=add_args)
  #else:
  #  mdl_dict['n'] = mdl_dict['o']

  if vconfig:
    mdl_dict['v'] = wosedon_plugin.WoSeDonPlugin(
      vconfig, mdir, verbose=verbose, argv=add_args)
  #else:
  #  mdl_dict['v'] = mdl_dict['o']

  return mdl_dict

'''
def add_annotation(models, rankings, out_cclfilepath):
  full_pos_mask = None
  available_models = models.keys()
  for (token, tagset), node_rank in rankings['o'].iteritems():
    if not full_pos_mask:
      full_pos_mask = corpus2.get_attribute_mask(tagset, '')
    if not node_rank or not token or not len(token.orth_utf8()):
      continue

    print len(token.lexemes())
    tag = token.get_preferred_lexeme(tagset)#.tag()
    # pos_mask = tag.get_masked(full_pos_mask)
    print ('Added annotation for token %s' % token.orth_utf8())

    # ref_pos = wsdctx.get_token_coarse_pos(token.clone())
    # refmodel = models[ref_pos] if ref_pos in available_models else models['o']
'''
def add_annotation(models, rankdict, out_cclfilepath):
  tagset = models['o'].config.resources().tagset()
  full_pos_mask = corpus2.get_attribute_mask(tagset, '')
  ctx  = ccc.WSDContext(tagset, [])

  for i, rctx in enumerate(rankdict['o']):
    for j, (token, node_rank) in enumerate(rctx):
      if not node_rank:
        continue

      md_ptr = token.get_metadata()
      if not md_ptr:
        token.create_metadata()
        md_ptr = token.get_metadata()

      # Coarse POS for token.
      tag = token.get_preferred_lexeme(tagset).tag()
      pos_mask = tag.get_masked(full_pos_mask)
      pos_str = ctx.convert_to_coarse_pos(
        tagset.tag_to_symbol_string(pos_mask))

      synset_rank = None
      fst_rank_synid = None
      fst_unitsstr = None
      if pos_str in rankdict:
        model = models[pos_str]
        node_rank = rankdict[pos_str][i][j][1] # w tym miejscu moze nie dzialac jesli zostanie ustawiony parametr alpha
        synset_rank = [(model.graph.get_node_attribute("synset", r[0]).synset_id, r[1]) 
                       for r in node_rank]
        # best synset ID
        fst_rank_synid = str(synset_rank[0][0])
        # best unit string
        fst_unitsstr = wosedon_base.make_unitstr(model.graph, node_rank[0][0])
      else:
        model = models['o']
        synset_rank = [(model.graph.get_node_attribute("synset", r[0]).synset_id, r[1]) 
                       for r in node_rank]
        # best synset ID
        fst_rank_synid = str(synset_rank[0][0])
        # best unit string
        fst_unitsstr = wosedon_base.make_unitstr(model.graph, node_rank[0][0])

      whole_rank_norm_str = wosedon_base.make_whole_rank_str(synset_rank)
      md_ptr.set_attribute('sense:ukb:syns_id', fst_rank_synid)
      md_ptr.set_attribute('sense:ukb:syns_rank', whole_rank_norm_str)
      md_ptr.set_attribute('sense:ukb:unitsstr', fst_unitsstr[:-1])

def run_disambiguation(models, args, in_cclfilepath, out_cclfilepath = None):
  """!
  @return dictionary with ranking for each model
  @rtype: List of tuples (token, [ranking, for, token])
  """

  rankdict = {}
  contextdict = {}
  for mkey, model in models.iteritems():
    rankings = []
    # make contextdict
    contextdict[mkey] = model.config.context(in_cclfilepath, in_cclfilepath)

    # run process
    run_wosedon._run_disambiguation(
      model.graph, 
      contextdict[mkey], 
      model.algorithm, 
      model.config,
      args.alpha,
      args.verbose,
      args.visualisation_dir,
      output_filename=None, 
      show_iterations=False,
      model_result_list=rankings)

    '''
    # convert to dictionary: token -> ranking
    rankdict[mkey] = dict()
    for rctx in rankings:
      for t, rank in rctx:
        rankdict[mkey][t, tagset] = rank
    '''
    rankdict[mkey] = rankings

  if out_cclfilepath:
    add_annotation(models, rankdict, out_cclfilepath)
    wosedon_base.write_ccl_file(models['o'].config, 
                                contextdict['o'].ccl_document(), 
                                out_cclfilepath)

  return rankdict


def main(argv=None):
  p = make_parser(__doc__)
  args = p.parse_args(argv)

  # additional arguments
  add_args = []
  if args.iterations:
    add_args.extend(['-it', args.iterations])
  if args.alpha:
    add_args.extend(['-a', args.alpha])

  # load the models
  modeldict = load_models(
    args.other_pos_cfg, args.noun_pos_cfg, args.verb_pos_cfg, 
    args.model_dir, add_args, args.verbose)

  if args.batch:
    with open(args.ccl, 'rt') as batch_file:
      for line in batch_file:
        ccl_filename = line.strip()
        out_ccl_filename = ccl_filename + ".wosedon.xml"
        if args.verbose:
          print >> sys.stderr, 'Running WoSeDoN for file:', ccl_filename
        # run disambiguation and add the annotations to text
        rankdict = run_disambiguation(modeldict, args, ccl_filename, out_ccl_filename)
  else:
    # run disambiguation and add the annotations to text
    rankdict = run_disambiguation(modeldict, args, args.ccl, args.out_file)


if __name__ == '__main__':
  main()