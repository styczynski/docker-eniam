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
import codecs, os, sys, argparse
import math

import domains as dom
import context.context as ccc
from npsemrel.carrot.db import db
from collections import defaultdict

def make_parser():
  desc = 'Dokonuje oceny UBK-wsd z wzorcowym oznakowaniem kpwr'
  parser = argparse.ArgumentParser(description = desc)
  parser.add_argument('-i', '--index-doc', dest = 'index_doc', required = True)
  parser.add_argument('-r', '--result-filename', dest = 'result_filename', required = True)
  parser.add_argument('-p', '--precision-filename', dest = 'precision_filename', required = True)
  parser.add_argument('-rc', '--recall-filename', dest = 'recall_filename', required = True)
  parser.add_argument('-d', '--db-config', dest = 'db_config', required = True)
  parser.add_argument('-t', '--tagset', dest = 'tagset', default = 'nkjp')
  parser.add_argument('-pr', '--percent-of-rank', dest = 'percent_of_rank', required = False)
  return parser

def make_evaluation(corpus_index_file, result_file, tagset, ann_map, dbconnection, corp_annotations, percent_of_rank):
  full_pos_mask = corpus2.get_attribute_mask(tagset, '')
  ctx  = ccc.WSDContext(tagset, [])

  with codecs.open(result_file, 'wt') as outfile:
    if percent_of_rank:
      outfile.write('POS;POS db;Manual synset ID;Manual synset string;WSD synset ID;WSD synset string;Correctness;WSD ranking of %s%%;Position in rank\n' % (percent_of_rank))
    else:
      outfile.write('POS;POS db;Manual synset ID;Manual synset string;WSD synset ID;WSD synset string;Correctness\n')

    with open(corpus_index_file, 'rt') as corpidx:
      for corpfile in corpidx:
        corpfile = os.path.join(os.path.dirname(corpus_index_file), corpfile).strip()
        print >> sys.stderr, 'Reading:', corpfile
        try:
          cclreader = corpus2.CclRelReader(tagset, corpfile, corpfile)
          document = cclreader.read()
          for paragraph in document.paragraphs(): 
            for sentence in paragraph.sentences(): 
              for token in sentence.tokens():
                md = token.get_metadata()
                if md:
                  WSD_tag = False
                  manual_tag = False
                  pos_id_synset = None

                  ukb_syn_id = md.get_attribute('sense:ukb:syns_id')
                  ukb_syn_unitsstr = md.get_attribute('sense:ukb:unitsstr')
                  ukb_syn_rank = md.get_attribute('sense:ukb:syns_rank')

                  # Sprawdzenie czy w korpusie, przy przy ustawionym
                  # procencie rankingu, ktory ma byc brany
                  # do oceny WSD, znajduje sie tag z rankingiem.
                  if ukb_syn_id and percent_of_rank and not ukb_syn_rank:
                    print >> sys.stderr, 'Brak tagu z rankingiem WSD w korpusie tekstu!'
                    exit(1)
                  
                  # Sprawdzenie czy dla analizowanego tokenu
                  # przypisana jest anotacja WSD.
                  if ukb_syn_id: 
                    WSD_tag = True

                  lemma = str(token.get_preferred_lexeme(tagset).lemma())

                  tag = token.get_preferred_lexeme(tagset).tag()
                  pos_mask = tag.get_masked(full_pos_mask)
                  pos_str = ctx.convert_to_coarse_pos(
                    tagset.tag_to_symbol_string(pos_mask))

                  attribs = md.attributes()
                  for attr_k, attr_v in attribs.iteritems():
                    if attr_k.startswith('sense:wsd_'):
                      if not ann_map.has_key(attr_v):
                        print >> sys.stderr, 'Brak klucza %s w slowniku!' % (attr_v)
                        continue

                      manual_tag = True
                      pos_db = ann_map[attr_v].values()[0][1]
                      man_syn_id = ', '.join([str(synset_id) for synset_id in ann_map[attr_v].iterkeys()])
                      man_syn_str = '|'.join([man_syn_str[0] for man_syn_str in [synset for synset in ann_map[attr_v].itervalues()]])

                      pos_id_synset = '%s;%s;%s' % (
                        pos_str,
                        man_syn_id,
                        man_syn_str)

                      if WSD_tag:
                        if percent_of_rank:
                          ukb_syn_list = [syn.strip() for syn in ukb_syn_rank.split(' ')]
                          number_of_selected_syn = int(math.ceil(float(len(ukb_syn_list)) / 100.0 * float(percent_of_rank)))
                          ukb_syn_list = ukb_syn_list[:number_of_selected_syn]

                          selected_syn = None
                          position_in_rank = None
                          for index, synset in enumerate(ukb_syn_list):
                            synset_id = int(synset.split('/')[0])
                            if ann_map[attr_v].has_key(synset_id):
                              selected_syn = synset_id
                              position_in_rank = index + 1
                              break

                          outfile.write('%s;%s;%s;%s;%s;%s;%d;%s;%d\n' % (
                            pos_str,
                            pos_db,
                            man_syn_id, 
                            man_syn_str,
                            str(selected_syn if selected_syn else ukb_syn_id), 
                            man_syn_str if selected_syn else ukb_syn_unitsstr,
                            1 if selected_syn else 0,
                            ukb_syn_list,
                            position_in_rank if position_in_rank else 0))
                        
                        else:
                          outfile.write('%s;%s;%s;%s;%s;%s;%d\n' % (
                            pos_str,
                            pos_db,
                            man_syn_id,
                            man_syn_str,
                            str(ukb_syn_id), 
                            ukb_syn_unitsstr, 
                            ann_map[attr_v].has_key(int(ukb_syn_id))))

                  if manual_tag:
                    if not corp_annotations.has_key(pos_id_synset):
                      corp_annotations[pos_id_synset] = [0, 0, 0]
                    corp_annotations[pos_id_synset][0] += 1
                    if WSD_tag:
                      corp_annotations[pos_id_synset][1] += 1
                  elif (not manual_tag) and WSD_tag:
                    pos_id_synset = '%s;%s;%s' % (
                      pos_str, 
                      str(ukb_syn_id), 
                      ukb_syn_unitsstr)
                    if not corp_annotations.has_key(pos_id_synset):
                      corp_annotations[pos_id_synset] = [0, 0, 0]
                    corp_annotations[pos_id_synset][2] += 1

        except Exception, e:
          print >> sys.stderr, 'Error: ', e

def precision(result_filename, precision_filename):
  words = {}
  with codecs.open(result_filename, 'rt', 'utf-8') as ifile:
    next(ifile)
    for line in ifile:
      spl_line = line.split(';')
      pos = spl_line[0]
      pos_db = spl_line[1]
      man_syn_id = spl_line[2]
      man_syn_str = spl_line[3]
      word = '%s;%s;%s;%s' % (pos_db, pos, man_syn_id, man_syn_str)
      correctness = int(spl_line[6])
      if not words.has_key(word):
        words[word] = [0, 0]
      words[word][correctness] += 1

  with codecs.open(precision_filename, 'wt', 'utf-8') as ofile:
    ofile.write('POS db;POS;Manual synset ID;Manual synset string;Incorrect;Correct;Precision\n')
    for word, correctness in words.iteritems():
      ofile.write('%s;%d;%d;%f\n' % (word, correctness[0], correctness[1], ((correctness[1] + 0.0) / (0.0 + sum(correctness)))))

def recall(corp_annotations, recall_filename):
  with codecs.open(recall_filename, 'wt') as ofile:
    ofile.write('POS;Manual synset ID;Manual synset string;All manual tags;Manual + WSD tags;Only WSD tags;Recall\n')
    for pos_id_synset, data in corp_annotations.iteritems():
      if data[0] == 0:
        rec = 0
      else:
        rec = (data[1] + 0.0) / (data[0] + 0.0)
      ofile.write('%s;%d;%d;%d;%f\n' % (pos_id_synset, data[0], data[1], data[2], rec))

def make_key(comment):
  return comment.replace('WSD', '').replace(' ', '').replace('#', '-').encode('utf-8')

def make_anotation_dictionary(dbconnection):
  ann_dict = {}

  print >> sys.stderr, 'Getting WSD comments from DB...',
  query = "SELECT SYN.comment, SYN.id, LU.lemma, LU.variant, LU.domain, LU.pos " \
          "FROM lexicalunit LU " \
          "JOIN unitandsynset UAS on (LU.ID = UAS.LEX_ID) " \
          "JOIN synset SYN on (SYN.id = UAS.SYN_ID) " \
          "WHERE SYN.comment LIKE '%wsd%';"
  cursor = dbconnection.cursor()
  cursor.execute(query)

  for row in cursor.fetchall():
    comments = row[0].split(';')[0].split(',')
    synset_id = int(row[1])

    lu_lemma = str(row[2].encode('utf-8')).replace(' ', '_')
    lu_pos = int(row[5])
    lu_variant = int(row[3])
    lu_domain = int(row[4])
    lu_domain_str = dom.DOMAINS[lu_domain] if dom.DOMAINS.has_key(lu_domain) else str(lu_domain)
    manual_synset_str = ('%s.%d(%d:%s) ' % \
                        (lu_lemma, 
                         lu_variant, 
                         lu_domain, 
                         lu_domain_str))

    for comment in comments:
      comment = make_key(comment)
      if ann_dict.has_key(comment):
        if ann_dict[comment].has_key(synset_id):
          ann_dict[comment][synset_id][0] += ' ' + manual_synset_str
          if lu_pos != ann_dict[comment][synset_id][1]:
            print >> sys.stderr, 'Dla synsetu', synset_id, 'wystapily rozne POSy!'
        else:
          ann_dict[comment][synset_id] = [manual_synset_str, lu_pos]
      else:
        ann_dict[comment] = defaultdict(list)
        ann_dict[comment][synset_id] = [manual_synset_str, lu_pos]
  print >> sys.stderr, ' Done!'

  return ann_dict

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

  annotations = make_anotation_dictionary(dbconnection)

  corp_annotations = {}
  make_evaluation(
    args.index_doc, 
    args.result_filename, 
    tagset, 
    annotations, 
    dbconnection,
    corp_annotations,
    args.percent_of_rank)

  precision(args.result_filename, args.precision_filename)
  recall(corp_annotations, args.recall_filename)


if __name__ == '__main__':
  main()
