from npsemrel.carrot.db import db
from collections import defaultdict
import codecs, os, sys, argparse

import platform, ctypes

if 'Linux' in platform.system():
    dlflags = sys.getdlopenflags()
    sys.setdlopenflags(dlflags | ctypes.RTLD_GLOBAL)

import maca

if 'Linux' in platform.system():
    # get back to default dlopen policy
    sys.setdlopenflags(dlflags)

import corpus2
from wcrft2 import Tagger as Wcrft2Tagger

def make_parser():
  desc = 'Generuje plik XML dla gloss z PLWN.'
  parser = argparse.ArgumentParser(description = desc)
  parser.add_argument('-d', '--db-config', dest = 'db_config', required = True)
  parser.add_argument('-i', '--in-gloss-file', dest = 'in_gloss_file', required = True)
  parser.add_argument('-o', '--out-ccl', dest = 'out_ccl', required = True)
  parser.add_argument('-t', '--tagset', dest = 'tagset', default = 'nkjp')
  return parser


def get_lu_on_gloss_dict(gloss_file):
  '''
  Format gloss_file:
  lu_1 [TABULACJA] gloss
  lu_2 [TABULACJA] gloss
  ...
  lu_N [TABULACJA] gloss
  '''
  lu_on_gloss_dict = defaultdict(set)

  with open(gloss_file, 'rt') as ifile:
    for line in ifile:
      lu_id = int(line.strip().split('\t')[0])
      gloss = line.strip().split('\t')[1]

      lu_on_gloss_dict[lu_id].add(gloss)

  return lu_on_gloss_dict

def get_bzn_synset_dict(dbconnection):
  bzn_synset_dict = defaultdict(set)

  query = "SELECT parent_id, child_id " \
          "FROM synsetrelation " \
          "WHERE rel_id=60;"
  cursor = dbconnection.cursor()
  cursor.execute(query)

  for row in cursor.fetchall():
    parent_id = int(row[0])
    child_id = int(row[1])
    bzn_synset_dict[parent_id].add(child_id)

  return bzn_synset_dict

def get_synset_on_lu_dict(dbconnection, bzn_synset_dict):
  synset_on_lu_dict = defaultdict(set)

  query = "SELECT SYN_ID, LEX_ID " \
          "FROM unitandsynset;"
  cursor = dbconnection.cursor()
  cursor.execute(query)

  for row in cursor.fetchall():
    syn_id = int(row[0])
    lu_id = int(row[1])
    synset_on_lu_dict[syn_id].add(lu_id)

  # uzupelnienie slownika synset_on_lu_dict
  # jesli pomiedzy synsetami zachodzi relacja bliskoznacznosci
  # to jednostki leksykalne sa przepisywane z jednego synsetu do drugiego
  for parent_id, child_set in bzn_synset_dict.iteritems():
    if not synset_on_lu_dict.has_key(parent_id):
      continue
    for child_id in child_set:
      if not synset_on_lu_dict.has_key(child_id):
        continue
      for lu_id in synset_on_lu_dict[child_id]:
        synset_on_lu_dict[parent_id].add(lu_id)

  return synset_on_lu_dict

def create_document(synset_on_lu_dict, lu_on_gloss_dict):
  tagger = Wcrft2Tagger("nkjp_e2.ini", "model_nkjp10_wcrft_e2")
  tagger.load_model()

  tagged_document = corpus2.Document()
  sentence_number = 0
  
  for synset_id, lu_set in synset_on_lu_dict.iteritems():
    tagged_chunk_ptr = corpus2.Chunk().clone_shared()
    tagged_chunk_ptr.set_attribute('id', str(synset_id))

    for lu_id in lu_set:
      merged_gloss = None
      if not lu_on_gloss_dict.has_key(lu_id):
        continue
      merged_gloss = ' '.join(list(lu_on_gloss_dict[lu_id]))
  
      maca_text_reader = maca.PlainTextReader.create_string_reader(merged_gloss, tagger.get_maca_config())
      for sentence in sentences(maca_text_reader):
        sentence_ptr = corpus2.AnnotatedSentence.wrap_sentence(sentence).clone_shared()
        sentence_ptr.set_id('s' + str(sentence_number) + ':' + str(lu_id))
        tagger.tag_sentence(sentence_ptr, False)
        tagged_chunk_ptr.append(sentence_ptr)
        sentence_number += 1

    if tagged_chunk_ptr.empty():
      continue

    tagged_document.add_paragraph(tagged_chunk_ptr)

  return tagged_document

def sentences(rdr):
  while True:
    sent = rdr.get_next_sentence()
    if not sent:
      break
    yield sent

def write_document(document, out_ccl, tagset):
  ccl_writer = corpus2.TokenWriter.create_path_writer(
    'ccl', 
    out_ccl, 
    tagset)
  for chunk in document.paragraphs():
    ccl_writer.write_chunk(chunk)
  ccl_writer.finish()
  del ccl_writer


def main(argv = None):
  parser = make_parser()
  args = parser.parse_args(argv)
  tagset = corpus2.get_named_tagset(args.tagset)

  print >> sys.stderr, 'Connecting to DB...',
  dbcon = db.DB()
  dbconnection = dbcon.connect(args.db_config)
  if not dbconnection:
    print >> sys.stderr, 'Cannot connect to DB!'
    exit(1)
  print >> sys.stderr, ' Done!'

  lu_on_gloss_dict = get_lu_on_gloss_dict(args.in_gloss_file)
  bzn_synset_dict = get_bzn_synset_dict(dbconnection)
  synset_on_lu_dict = get_synset_on_lu_dict(dbconnection, bzn_synset_dict)
  
  tagged_document = create_document(synset_on_lu_dict, lu_on_gloss_dict)
  write_document(tagged_document, args.out_ccl, tagset)


if __name__ == '__main__':
  main()