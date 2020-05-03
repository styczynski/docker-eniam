# Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Nov 2016.
# This file is intended to check the (partially tagged) NKJP1M frequency list against list of exce-
# ptions from morphological rules derived from SGJP.
# If you want to use this, review the end of this file (filenames, column structure) and run with python3.

import re

# just ripped from compare_morphosyn.py, guess it'll be better to keep those scripts self-contained
# note that liberal_tagcomp is mainly suitable for checking NKJP against SGJP, when checking
# a resource obeying more SJGP'ish tagging convention the strict_tagcomp will be better
def strict_tagcomp(tag1, tag2):
    tag1_items = tag1.split(':')
    tag2_items = tag2.split(':')

    if (tag1_items[0] != tag2_items[0] # POS
            or len(tag1_items) != len(tag2_items)):
        return False

    for (i, item) in enumerate(tag1_items):
        if not item in tag2_items[i].split('.'):
            return False

    return True

def liberal_tagcomp(tag1, tag2):
    tag1_items = tag1.split(':')
    tag2_items = tag2.split(':')

    if (tag1_items[0] != tag2_items[0] # POS
            or len(tag1_items) != len(tag2_items)):
        return False

    for (i, item) in enumerate(tag1_items):
        # remove tags n1, f1...
        item = re.sub(r'(n1|n2|n3)', 'n', item)
        model = re.sub(r'(n1|n2|n3|p2|p3)', 'n', tag2_items[i]).split('.')
        if not item in model and model[0] != '_': # underscore as a catchall
            return False

    return True

# the bulk of the following ripped from check_rule_compos.py
def esccurl(string) :
    "Escape the curly brackets in the string, for using it with the string formatter."
    return string.replace('{', '{{').replace('}', '}}')

alt_idx = dict() # indexed by data[0] - word form

with open('../resources/SGJP/alt.tab') as alt_src:
    for line in alt_src:
            line = line.strip()
            data = line.split('\t')
            if len(data) != 3:
                print('Skipped line in the alt list: '+line)
                continue
            # handle lemmas with subclassification after colon
            if data[1].find(':') != -1 and data[1] != ':':
                data[1] = data[1][: data[1].find(':')]
            # each entry consists of 0 - list of lemmas, 1 - list of tags
            if not data[0] in alt_idx:
                alt_idx[data[0]] = [[data[1]], [data[2]]]
            else:
                alt_idx[data[0]][0].append(data[1])
                alt_idx[data[0]][1].append(data[2])

with open('../resources/NKJP1M/NKJP1M-tagged-frequency.tab') as inp:
    with open('freq_with_alt.tab', 'w+') as out:
        for line in inp:
            line = line.strip()
            data = line.split('\t')
            if len(data) != 8: # column count of TAGGED frequency list
                print('Skipped line in the list: '+line)
                continue

            # The following was added to work on partially done tagged frequency, to get rid of the
            # previous COMPOS classification. Otherwise we'd want to use something like this:
            # fmt = esccurl(line) + '\t{0}' # simple format string, applicable to raw frequency list
            # previous COMPOS column is in data[4], so we skip it below
            fmt = esccurl('\t'.join(data[0:4])) + '\t{0}\t' + esccurl('\t'.join(data[5:]))

            matched = False
            if data[0] in alt_idx:
                tagcomps = list(map(lambda x: liberal_tagcomp(data[2], x), alt_idx[data[0]][1]))
                tagnum = True in tagcomps and tagcomps.index(True)
                # (make sure that if lemma is matching, it belongs to the matching tag)
                if tagnum != -1 and tagnum != False and alt_idx[data[0]][0][tagnum] == data[1]:
                    print(fmt.format('COMPOS-ALT'), file=out)
                    matched = True
            # try again with lowering word form and lemma:
            if not matched and data[0].lower() in alt_idx:
                tagcomps = list(map(lambda x: liberal_tagcomp(data[2], x), # data[2] - tag stays the same
                                    alt_idx[data[0].lower()][1]))
                tagnum = True in tagcomps and tagcomps.index(True)
                if tagnum != -1 and tagnum != False and alt_idx[data[0].lower()][0][tagnum] == data[1].lower():
                    print(fmt.format('COMPOS-LWR-ALT'), file=out)
                    matched = True
            if not matched:
                print(line, file=out)
