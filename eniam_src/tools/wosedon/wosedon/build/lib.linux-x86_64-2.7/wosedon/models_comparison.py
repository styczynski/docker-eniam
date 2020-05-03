import corpus2
import codecs, os, sys, argparse

import domains as dom
import context.context as ccc
from npsemrel.carrot.db import db
from collections import defaultdict

def make_parser():
  desc = 'Narzedzie do porownywania wynikow WSD dla roznych modeli.'
  parser = argparse.ArgumentParser(description = desc)
  parser.add_argument('-o', '--out-file', dest = 'out_file', required = True)
  parser.add_argument('-i', '--input-files', nargs = '*', action = 'append', 
                      dest = 'in_files', required = True)
  parser.add_argument('-d', '--db-config', dest = 'db_config', required = True)
  parser.add_argument('-t', '--tagset', dest = 'tagset', default = 'nkjp')
  parser.add_argument('-e', '--only-incorrect', dest = 'only_incorrect', 
                      action = 'store_true')
  return parser

def get_models_comparsion_data(models_to_compare, tagset, ann_map):
  full_pos_mask = corpus2.get_attribute_mask(tagset, '')
  ctx  = ccc.WSDContext(tagset, [])

  statistic_dict = defaultdict(set)
  models_list = []

  for model_number, model in enumerate(models_to_compare):
    models_list.append((model_number, os.path.dirname(model)))
    with open(model, 'rt') as corpidx:
      for corpfile in corpidx:
        corpfile_full_path = os.path.join(os.path.dirname(model), corpfile).strip()
        print >> sys.stderr, 'Reading:', corpfile_full_path
        try:
          cclreader = corpus2.CclRelReader(tagset, corpfile_full_path, corpfile_full_path)
          document = cclreader.read()
          for paragraph in document.paragraphs():
            for sentence in paragraph.sentences():
              for token in sentence.tokens():
                md = token.get_metadata()
                if md:
                  ukb_syn_id = md.get_attribute('sense:ukb:syns_id')
                  ukb_syn_unitsstr = md.get_attribute('sense:ukb:unitsstr')
                  ukb_syn_rank = md.get_attribute('sense:ukb:syns_rank')

                  if not ukb_syn_id:
                    continue

                  attribs = md.attributes()
                  for attr_k, attr_v in attribs.iteritems():
                    if attr_k.startswith('sense:wsd_'):
                      if not ann_map.has_key(attr_v):
                        print >> sys.stderr, 'Brak klucza %s w slowniku!' % (attr_v)
                      else:
                        # Lemat
                        lemma = str(token.get_preferred_lexeme(tagset).lemma())
                        # POS
                        tag = token.get_preferred_lexeme(tagset).tag()
                        pos_mask = tag.get_masked(full_pos_mask)
                        pos_str = ctx.convert_to_coarse_pos(
                          tagset.tag_to_symbol_string(pos_mask))

                        key = (corpfile,
                               sentence.id(),
                               lemma,
                               pos_str,
                               attr_v,
                               ', '.join(str(syn_id) for syn_id in ann_map[attr_v].keys()))

                        value = (model_number,
                                 ukb_syn_unitsstr,
                                 ukb_syn_id,
                                 ukb_syn_rank)

                        statistic_dict[key].add(value)
        except Exception, e:
          print >> sys.stderr, 'Error: ', e

  return (statistic_dict, models_list)

def models_comparsion(statistic_dict, models_list, out_file):
  with open(out_file, 'wt') as ofile:
    set_column_name(ofile, models_list)

    for key, value_set in statistic_dict.iteritems():
      union = set()
      for value in value_set:
        union.add(value[2])
      if len(union) > 1:
        set_data(ofile, key, value_set)

def models_comparsion_only_incorrect(statistic_dict, models_list, out_file):
  with open(out_file, 'wt') as ofile:
    set_column_name(ofile, models_list)

    for key, value_set in statistic_dict.iteritems():
      correct_syn_id_list = [int(syn_id) for syn_id in key[5].split(', ')]
      for value in value_set:
        if not int(value[2]) in correct_syn_id_list:
          set_data(ofile, key, value_set)
          break

def set_column_name(ofile, models_list):
  models_names = ';;;'.join([model_pth for (model_number, model_pth) in models_list])
  models_data = ';'.join(['WSD synset string;WSD synset ID;Ranking' for i in range(len(models_list))])

  ofile.write('%s;%s;%s;%s;%s;%s;%s\n;;;;;;%s\n' % (
              'Document',
              'Sentence ID',
              'Lemma',
              'POS',
              'Manual synset ID',
              'Manual synset string',
              models_names,
              models_data))

def set_data(ofile, key, value_set):
  sort_value_list = sorted(list(value_set), key = lambda model: model[0])
  models_data = ';'.join(['%s;%s;%s' % (value[1], value[2], value[3]) if value_number == value[0] else ';;' for value_number, value in enumerate(sort_value_list)])

  ofile.write('%s;%s;%s;%s;%s;%s;%s\n' % (
              key[0].strip(),
              key[1],
              key[2],
              key[3],
              key[5],
              key[4],
              models_data))

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

  models_to_compare = []
  for models_list in args.in_files:
    for model in models_list:
      models_to_compare.append(model)

  annotations = make_anotation_dictionary(dbconnection)

  statistic_dict = defaultdict(set)
  models_list = []
  (statistic_dict, models_list) = get_models_comparsion_data(models_to_compare, tagset, annotations)

  if args.only_incorrect:
    models_comparsion_only_incorrect(statistic_dict, models_list, args.out_file)
  else:
    models_comparsion(statistic_dict, models_list, args.out_file)

if __name__ == '__main__':
  main()