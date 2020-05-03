#! /usr/bin/python
# *-* coding: utf-8 *-*

from __future__ import print_function
from concrafteusz import Concraft

import codecs
import argparse
import glob
import morfeusz2
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument('files', help="nazwa pliku do analizy (plain text)", nargs='*')
parser.add_argument('-c', '--conll', action='store_true', default=False, help='Wyniki w formacie CoNLL dla parsera zależnościowego.')
args = parser.parse_args()
make_conll = parser.parse_args().conll

#concraftpath = './concraft-dag2-pl'

morfeusz = morfeusz2.Morfeusz(
    dict_name='sgjp',
    expand_tags=True,
    generate=False,
)

tager = Concraft(
    model_name = 'concraft-pl-model-180317.gz',
    model_path = './',
    port = 3005,
    core = 5
)


def delrozp(lemat):
    if lemat != ':':
        lemat = lemat.split(':')[0]
    return(lemat)

if make_conll:
    COMPs = ('przeto', u'jakoż', u'toteż', 'tedy', 'to', 'ergo', 'czym', 'jakokolwiek', u'więc', 'zatem')

    d = {
        'adjnum' : 'adj',
        'advnum' : 'adv',
        'part' : 'qub',
        'fut' : 'bedzie',
        'plusq' : 'praet',
        'frag' : 'burk'
    }

    def conv(tag):
        tag = tag.split(':')
        if tag[0] == 'adjp':
            tag = ['adjp',]
        elif tag[0] == 'ppron12':
            del tag[3] 
        elif tag[0] in d:
            tag[0] = d[tag[0]]
        tag = ':'.join(tag)
        return(tag)

    sent = []

try:
    for plainf in args.files:
        if make_conll:
            taggedf = plainf.replace('.', '_') + '_tagged.conll'
        else:
            taggedf = plainf.replace('.', '_') + '_tagged.dag'
        with codecs.open(plainf, mode='r', encoding='utf-8') as pf:
            with codecs.open(taggedf, mode='w', encoding='utf-8') as tf:
                print('tagging file: ' + str(plainf))
                for line in pf:
                    if line != '\n':
                        analysis = morfeusz.analyse(line)
                        tagged = tager.tag(analysis)
                        for item in tagged:
                            num1, num2, (forma, lemat, tag, posp, kwal), prob, eos, disamb = item
                            lemat = delrozp(lemat)
                            if make_conll:
                                if tag == 'conj' and lemat in COMPs:
                                    tag = 'comp'
                                if disamb == 'disamb':
                                    if eos != 'eos':
                                        sent.append((forma, lemat, conv(tag)))
                                    else:
                                        sent.append((forma, lemat, tag))
                                        for count, item in enumerate(sent, 1):
                                            ctag = item[2].split(':')[0]
                                            feat = '|'.join(item[2].split(':')[1:]) if len(item[2].split(':')) > 1 else '_'
                                            tupla = (str(count), item[0], item[1], ctag, ctag, feat, '_', '_', '_', '_')
                                            print('\t'.join(tupla),file=tf)
                                        print('', file=tf)
                                        sent = []
                            else:
                                print('\t'.join((str(num1), str(num2), forma, lemat, tag, ','.join(posp), ','.join(kwal), str(prob), eos if eos else '', disamb if disamb else '')), file=tf)
finally:
    tager.done()
