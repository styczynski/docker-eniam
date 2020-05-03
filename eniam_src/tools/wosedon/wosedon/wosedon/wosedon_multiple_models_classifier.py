# -*- coding: utf-8 -*-

import argparse
try:
  import argcomplete
except ImportError:
  argcomplete = None

import codecs, os, sys
import operator
from collections import defaultdict

import threading
import corpus2

from wosedon.wosedon_plugin import WoSeDonPlugin
import domains as dom

def make_parser():
  desc = 'Klasyfikator podejmujący decyzje na podstawie decyzji wielu modeli.'
  aparser = argparse.ArgumentParser(description=desc)

  aparser.add_argument('-f', '--cclfile', dest='ccl', required=True,
                        type=str, help='Ccl file.')

  aparser.add_argument('-c', '--config-files', nargs='*', action='append',
                        dest='config_files', required=True, 
                        help='WoSeDon config files.')

  aparser.add_argument('-md', '--models-dir', nargs='*', action='append',
                       dest='models_dir', required=False, 
                       help=(
                        'Output directory where the models will be stored. '
                        'If is not given then the models will not be stored. '
                        'If output directory is given, and the models '
                        'exists into directory, then tey will be loaded.'))

  aparser.add_argument('-V', '--verbose', dest='verbose', action='store_true',
                       help='If -V is set then verbose mode is enabled.')

  if argcomplete:
    argcomplete.autocomplete(aparser)
  return aparser

def get_files_paths_list(old_files_paths):
  new_files_paths_list = []
  for files_paths_list in old_files_paths:
    for file_path in files_paths_list:
      new_files_paths_list.append(file_path)
  return new_files_paths_list

def get_fpaths_list(fpaths_file):
  fpaths_list = []
  with codecs.open(fpaths_file, 'r') as ifile:
    for line in ifile:
      corpfile_full_path = os.path.join(os.path.dirname(fpaths_file), line).strip()
      fpaths_list.append(corpfile_full_path)
  return fpaths_list

def vote(best_nodes_list, model_object_NOUN, model_object_VERB):
  # identyfikator synsetu dla modelu zwracajacego najwyzsza
  # precyzje ujednoznaczniania dla rzeczownikow
  synset_id_NOUN = None
  # identyfikator synsetu dla modelu zwracajacego najwyzsza
  # precyzje ujednoznaczniania dla czasownikow
  synset_id_VERB = None

  # czesc mowy
  POS = None

  # slownik zbierajacy liczbe wystapien identyfikatorow synsetow,
  # ktore zostaly przypisane jako najlepsze w procesie WSD
  # dla kazdedgo modelu
  most_frequent_node_dict = {}

  # slownik w formacie: synset_id: synset_str
  synset_dict = {}

  for node, model_object in best_nodes_list:
    graph = model_object._g
    synset_id = graph.get_node_attribute("synset", node).synset_id

    POS = get_pos_int(graph, node)

    if model_object is model_object_NOUN:
      synset_id_NOUN = synset_id
    elif model_object is model_object_VERB:
      synset_id_VERB = synset_id

    if synset_id not in most_frequent_node_dict:
      most_frequent_node_dict[synset_id] = 0
    most_frequent_node_dict[synset_id] += 1

    if synset_id not in synset_dict:
      synset_str = make_unitstr(graph, node)
      synset_dict[synset_id] = synset_str

  synset_rank = sorted(most_frequent_node_dict.items(), key=operator.itemgetter(1), reverse=True)
  win_synset_id = synset_rank[0][0]
  win_synset_str = None

  # heurystyka: jesli 2 pierwsze synsety w synset_rank maja ta sama
  #             liczbe wystapien jako synset zwyciezca brany jest synset
  #             nalezacy do modelu dla odpowiedniej czesci mowy
  if len(synset_rank) > 1:
    if (POS == 1 or POS == 2) and synset_rank[0][1] == synset_rank[1][1]:
      win_synset_id = synset_id_VERB if POS == 1 else synset_id_NOUN

  win_synset_str = synset_dict[win_synset_id]

  return win_synset_id, win_synset_str, synset_rank

def make_unitstr(graph, node):
  fst_unitsstr = ''
  for lu in graph.get_node_attribute("synset", node).lu_set:
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

def get_pos_int(graph, node):
  return next(iter(graph.get_node_attribute("synset", node).lu_set)).pos

def get_synset_rank_str(synset_rank):
  return ' '.join(['{0}/{1}'.format(synset_id, num_of_occur) \
                   for (synset_id, num_of_occur) in synset_rank])

def add_annotations_from_rank(token, synset_id, synset_str, synset_rank):
  md_ptr = token.get_metadata()
  if not md_ptr:
    token.create_metadata()
    md_ptr = token.get_metadata()

  md_ptr.set_attribute('sense:ukb:syns_id', str(synset_id))
  md_ptr.set_attribute('sense:ukb:syns_rank', get_synset_rank_str(synset_rank))
  md_ptr.set_attribute('sense:ukb:unitsstr', synset_str[:-1])

def write_ccl_file(cfg, ccl_document, outccl):
  writer = (
    corpus2.TokenWriter.create_stdout_writer('ccl', cfg.resources().tagset())
      if outccl[0] == '-' \
      else \
        corpus2.TokenWriter.create_path_writer(\
          'ccl', outccl, cfg.resources().tagset()))
  for paragraph in ccl_document.paragraphs():
    writer.write_chunk(paragraph)

def run_wosedon_multiple_models_classifier(config_files, models_dir, fpaths, verbose):
  models_objects_list = []
  for i, cfg in enumerate(config_files):
    print i
    print 'a'
    wsdlg = WoSeDonPlugin(cfg, models_dir[i] if models_dir else None, verbose)
    print 'b'
    models_objects_list.append(wsdlg)
    print 'c'

  for inccl in fpaths:
    if verbose:
      print >> sys.stderr, 'Running WoSeDoN for file:', inccl

    jobs = []
    threads_result_dict = {}
    for model_object in models_objects_list:
      threads_result_dict[model_object] = []
      thread = threading.Thread(target=model_object.run_wosedon(
                                  inccl, None, None, 
                                  threads_result_dict[model_object]))
      jobs.append(thread)
    for j in jobs:
      j.start()
    for j in jobs:
      j.join()

    models_objects = threads_result_dict.keys()
    first_model_object = models_objects[0]
    for i, ctx_rank_list in enumerate(threads_result_dict[first_model_object]):
      for j, (token, node_rank) in enumerate(ctx_rank_list):
        if node_rank:
          best_node = node_rank[0][0]
          best_nodes_list = [(best_node, first_model_object)]
          for model_object in models_objects[1:]:
            best_node = threads_result_dict[model_object][i][j][1][0][0]
            best_nodes_list.append((best_node, model_object))
          (win_synset_id, win_synset_str, synset_rank) = vote(best_nodes_list, \
                                                              models_objects_list[0], \
                                                              models_objects_list[1])
          add_annotations_from_rank(token, win_synset_id, win_synset_str, synset_rank)

    cfg = first_model_object._wosedon_cfg
    context = first_model_object._context
    out_ccl_filename = inccl.strip() + ".wosedon.xml"
    write_ccl_file(cfg, context.ccl_document(), out_ccl_filename)


class DifferentConfigAndModel(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return repr(self.message)


def main(argv = None):
  p = make_parser()
  args = p.parse_args(argv)

  config_files = get_files_paths_list(args.config_files)
  models_dir = get_files_paths_list(args.models_dir) if args.models_dir else []

  try:
    if args.models_dir and not len(config_files) == len(models_dir):
      raise DifferentConfigAndModel('Liczba podanych na wejciu plikow ' \
                                    'konfiguracyjnych jest ' \
                                    'rozna od liczby plikow z modelami!')
  except DifferentConfigAndModel as e:
    exit(e.message)

  fpaths = get_fpaths_list(args.ccl)

  run_wosedon_multiple_models_classifier(config_files, models_dir, fpaths, args.verbose)


if __name__ == '__main__':
  main()
