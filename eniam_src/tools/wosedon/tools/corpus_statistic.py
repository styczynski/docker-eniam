import corpus2
import codecs, os, sys, argparse
from npsemrel.carrot.db import db
from collections import defaultdict


def make_parser():
  desc = 'Narzedzie generujace statystyke korpusu (liczba znaczen przypadajaca na slowa).'
  parser = argparse.ArgumentParser(description = desc)
  parser.add_argument('-i', '--index-doc', dest = 'index_doc', required = True)
  parser.add_argument('-r', '--result-filename', dest = 'result_filename', required = True)
  parser.add_argument('-d', '--db-config', dest = 'db_config', required = True)
  parser.add_argument('-t', '--tagset', dest = 'tagset', default = 'nkjp')
  parser.add_argument('-S', '--sentence', dest = 'sentence', action = 'store_true', required = False)
  parser.add_argument('-m', '--manual-senses-statistic', dest = 'manual_senses_statistic', action = 'store_true', required = False)
  return parser


def convert_to_num_pos(pl_pos):
  noun_pl_pos = ['subst', 'depr', 'ger']
  adj_pl_pos  = ['adj', 'adja', 'adjp', 'adjc']
  verb_pl_pos = ['fin', 'bedzie', 'praet', 'impt', \
                 'inf', 'pcon', 'pant', 'imps', \
                 'winien', 'pred', 'pact', 'ppas', 'pred']
  pos_int = None
  if pl_pos in noun_pl_pos:
    pos_int = 2
  elif pl_pos in adj_pl_pos:
    pos_int = 4
  elif pl_pos in verb_pl_pos:
    pos_int = 1
  return pos_int

def get_number_of_senses(lemma, pos_int, dbconnection):
  query = 'SELECT LU.lemma, LU.pos, UAS.SYN_ID ' \
          'FROM lexicalunit LU ' \
          'JOIN unitandsynset UAS ON (LU.id = UAS.LEX_ID) ' \
          'WHERE LU.lemma = BINARY "' + lemma + '" and LU.pos = ' + str(pos_int) + ';'
  cursor = dbconnection.cursor()
  cursor.execute(query)
  return len(cursor.fetchall())

def make_statistic(corpus_index_file, result_file, tagset, dbconnection, is_sentence):
  statistic_dict = {}
  max_num_of_syn = 0

  full_pos_mask = corpus2.get_attribute_mask(tagset, '')
  with open(corpus_index_file, 'rt') as corpidx:
    for corpfile in corpidx:
      corpfile_path = os.path.join(os.path.dirname(corpus_index_file), corpfile).strip()
      print >> sys.stderr, 'Reading:', corpfile_path
      try:
        cclreader = corpus2.CclRelReader(tagset, corpfile_path, corpfile_path)
        document = cclreader.read()
        
        statistic_dict[corpfile] = {}
        for paragraph in document.paragraphs(): 
          for sentence in paragraph.sentences():
            statistic_dict[corpfile][sentence.id()] = {}
            for token in sentence.tokens():
              # Lemma
              lemma = str(token.get_preferred_lexeme(tagset).lemma())
              # POS
              tag = token.get_preferred_lexeme(tagset).tag()
              pos_mask = tag.get_masked(full_pos_mask)
              pos_int = convert_to_num_pos(tagset.tag_to_symbol_string(pos_mask))
              if not pos_int:
                print >> sys.stderr, 'Niepoprawny POS dla lematu', lemma, '!'
                continue
              # Number of senses
              num_of_syn = get_number_of_senses(lemma, pos_int, dbconnection)
              if statistic_dict[corpfile][sentence.id()].has_key(num_of_syn):
                statistic_dict[corpfile][sentence.id()][num_of_syn] += 1
              else:
                statistic_dict[corpfile][sentence.id()][num_of_syn] = 1
                if num_of_syn > max_num_of_syn:
                  max_num_of_syn = num_of_syn
      except Exception, e:
        print >> sys.stderr, 'Error: ', e

  with codecs.open(result_file, 'wt') as outfile:
    if is_sentence:
      num_of_syn_list = [0] * (max_num_of_syn + 1)
      num_of_syn_str = ';'.join(str(i) for i in xrange(max_num_of_syn + 1))

      outfile.write('Nazwa dokumentu;Numer zdania;%s\n' % num_of_syn_str)
      for corpfile, sentence_dict in statistic_dict.iteritems():
        for sentence, num_of_syn_dict in sentence_dict.iteritems():
          for num_of_syn, num_of_lemmas in enumerate(num_of_syn_list):
            num_of_syn_list[num_of_syn] = num_of_syn_dict[num_of_syn] if num_of_syn_dict.has_key(num_of_syn) else 0
          num_of_syn_str = ';'.join(str(num_of_lemmas) for num_of_lemmas in num_of_syn_list)
          outfile.write('%s;%s;%s\n' % (corpfile.strip(), sentence, num_of_syn_str))
    else:
      num_of_syn_str = ';'.join(str(i) for i in xrange(max_num_of_syn + 1))
      
      outfile.write('Nazwa dokumentu;%s\n' % num_of_syn_str)
      for corpfile, sentence_dict in statistic_dict.iteritems():
        num_of_syn_list = [0] * (max_num_of_syn + 1)
        for num_of_syn, num_of_lemmas in enumerate(num_of_syn_list):
          for sentence, num_of_syn_dict in sentence_dict.iteritems():
            if num_of_syn_dict.has_key(num_of_syn):
              num_of_syn_list[num_of_syn] += num_of_syn_dict[num_of_syn]
        num_of_syn_str = ';'.join(str(num_of_lemmas) for num_of_lemmas in num_of_syn_list)
        outfile.write('%s;%s\n' % (corpfile.strip(), num_of_syn_str))

def make_manual_senses_statistic(corpus_index_file, result_file, tagset, dbconnection, is_sentence):
  ann_dict = make_anotation_dictionary(dbconnection)

  manual_statistic_dict = {}

  full_pos_mask = corpus2.get_attribute_mask(tagset, '')
  with open(corpus_index_file, 'rt') as corpidx:
    for corpfile in corpidx:
      corpfile_path = os.path.join(os.path.dirname(corpus_index_file), corpfile).strip()
      print >> sys.stderr, 'Reading:', corpfile_path
      try:
        cclreader = corpus2.CclRelReader(tagset, corpfile_path, corpfile_path)
        document = cclreader.read()
        
        manual_statistic_dict[corpfile] = {}
        for paragraph in document.paragraphs(): 
          for sentence in paragraph.sentences():
            manual_statistic_dict[corpfile][sentence.id()] = defaultdict(set)
            for token in sentence.tokens():
              # Manual sense
              md = token.get_metadata()
              if md:
                attribs = md.attributes()
                for attr_k, attr_v in attribs.iteritems():
                  if attr_k.startswith('sense:wsd_'):
                    if not ann_dict.has_key(attr_v):
                      print >> sys.stderr, 'Brak klucza %s w PLWN!' % (attr_v)
                      continue

                    # Lemma
                    lemma = str(token.get_preferred_lexeme(tagset).lemma())

                    # POS
                    tag = token.get_preferred_lexeme(tagset).tag()
                    pos_mask = tag.get_masked(full_pos_mask)
                    pos_int = convert_to_num_pos(tagset.tag_to_symbol_string(pos_mask))
                    if not pos_int:
                      print >> sys.stderr, 'Niepoprawny POS dla lematu', lemma, '!'
                      continue

                    manual_statistic_dict[corpfile][sentence.id()][(lemma, pos_int)].add(attr_v)
      except Exception, e:
        print >> sys.stderr, 'Error: ', e

  statistic_dict = {}
  max_num_of_syn = 0
  for corpfile, sentence_dict in manual_statistic_dict.iteritems():
    statistic_dict[corpfile] = {}
    for sentence, lemma_pos_int_dict in sentence_dict.iteritems():
      statistic_dict[corpfile][sentence] = {}
      for manual_senses_set in lemma_pos_int_dict.itervalues():
        num_of_syn = len(manual_senses_set)
        if statistic_dict[corpfile][sentence].has_key(num_of_syn):
          statistic_dict[corpfile][sentence][num_of_syn] += 1
        else:
          statistic_dict[corpfile][sentence][num_of_syn] = 1
          if num_of_syn > max_num_of_syn:
            max_num_of_syn = num_of_syn

  with codecs.open(result_file, 'wt') as outfile:
    if is_sentence:
      num_of_syn_list = [0] * (max_num_of_syn + 1)
      num_of_syn_str = ';'.join(str(i) for i in xrange(max_num_of_syn + 1))

      outfile.write('Nazwa dokumentu;Numer zdania;%s\n' % num_of_syn_str)
      for corpfile, sentence_dict in statistic_dict.iteritems():
        for sentence, num_of_syn_dict in sentence_dict.iteritems():
          for num_of_syn, num_of_lemmas in enumerate(num_of_syn_list):
            num_of_syn_list[num_of_syn] = num_of_syn_dict[num_of_syn] if num_of_syn_dict.has_key(num_of_syn) else 0
          num_of_syn_str = ';'.join(str(num_of_lemmas) for num_of_lemmas in num_of_syn_list)
          outfile.write('%s;%s;%s\n' % (corpfile.strip(), sentence, num_of_syn_str))
    else:
      num_of_syn_str = ';'.join(str(i) for i in xrange(max_num_of_syn + 1))
      
      outfile.write('Nazwa dokumentu;%s\n' % num_of_syn_str)
      for corpfile, sentence_dict in statistic_dict.iteritems():
        num_of_syn_list = [0] * (max_num_of_syn + 1)
        for num_of_syn, num_of_lemmas in enumerate(num_of_syn_list):
          for sentence, num_of_syn_dict in sentence_dict.iteritems():
            if num_of_syn_dict.has_key(num_of_syn):
              num_of_syn_list[num_of_syn] += num_of_syn_dict[num_of_syn]
        num_of_syn_str = ';'.join(str(num_of_lemmas) for num_of_lemmas in num_of_syn_list)
        outfile.write('%s;%s\n' % (corpfile.strip(), num_of_syn_str))

def make_key(comment):
  return comment.replace('WSD', '').replace(' ', '').replace('#', '-').encode('utf-8')

def make_anotation_dictionary(dbconnection):
  ann_dict = defaultdict(set)

  print >> sys.stderr, 'Getting WSD comments from DB...',
  query = "SELECT SYN.comment, SYN.id " \
          "FROM synset SYN " \
          "WHERE SYN.comment LIKE '%wsd%';"
  cursor = dbconnection.cursor()
  cursor.execute(query)

  for row in cursor.fetchall():
    comments = row[0].split(';')[0].split(',')
    synset_id = int(row[1])
    for comment in comments:
      comment = make_key(comment)
      ann_dict[comment].add(synset_id)
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

  if args.manual_senses_statistic:
    make_manual_senses_statistic(args.index_doc, args.result_filename, tagset, dbconnection, args.sentence)
  else:
    make_statistic(args.index_doc, args.result_filename, tagset, dbconnection, args.sentence)


if __name__ == '__main__':
  main()