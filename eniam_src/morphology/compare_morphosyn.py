# Blame Szymon Rutkowski - szymon@szymonrutkowski.pl - Oct 2016.
#
# Given a frequency list and groundtruth dictionary, tag the entries on the frequency list with some
# automatic tags (can be seen at the end of this file).
#
# Run from Python3, with -i (inspect option), eg. `python3 -i compare_morphosyn.py`.
# Then invoke something like (with # representing Python prompt):
# # sgjp = load_sgjp('../../NLP resources/sgjp-20160724.tab')
# # nkjp = load_nkjp('../resources/NKJP1M/NKJP1M-frequency.tab')
# # notmatching(nkjp, sgjp, liberal_tagcomp, 'raw_tagged_frequency.tab') # (may take a while)
# # ^D # Ctrl-D when done
# The last argument points the result file, liberal_tagcomp is the most sane tag comparing function.

import functools
import re
import unicodedata

def load_sgjp(fname):
    sgjp = dict()
    with open(fname) as inp:
        for line in inp:
            data = line.strip().split('\t')

            if len(data) < 3:
                print('Skipped line: ' + line.strip())
                continue

            word_form = data[0]

            lemma = ''
            lemma_sub = ''
            if data[1] == ':':
                lemma = [':']
            else:
                lemma = data[1].split(":")[0] # lemma subidentifier
                if len(data[1].split(":")) > 1:
                    lemma_sub = data[1].split(":")[1]
            if word_form.find('_') == -1:
                lemma = lemma.replace('_', ' ')

            tags = data[2]

            notes = ''
            if len(data) == 4:
                notes = data[3]

            if lemma in sgjp:
                sgjp[lemma].append([word_form, tags, notes])
            else:
                sgjp[lemma] = [ [word_form, tags, notes, lemma_sub] ]
    return sgjp


def load_nkjp(fname):
    nkjp = []
    with open(fname) as inp:
        nkjp = inp.read().split('\n')
    for (n, line) in enumerate(nkjp):
        nkjp[n] = nkjp[n].split('\t') # word_form, lemma, tags, freq
        if len(nkjp[n]) != 5:
            print('Skipped line: ' + str(n))
            del nkjp[n]
    return nkjp

def naive_tagcomp(tag1, tag2):
    return (tag1 == tag2)

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

def compare_entries(nkjp_entry, sgjp_forms, tagcomp_func):
    found = False
    case1 = False
    case2 = False
    case3 = False
    for (s, sgjp_form) in enumerate(sgjp_forms):
        nkjp_word = nkjp_entry[0]
        nkjp_tag = re.sub(r':$', '', nkjp_entry[2])
        if nkjp_tag !=  nkjp_entry[2]:
            print("Corrected tag %s for %s %s" % (nkjp_entry[2], nkjp_entry[0], nkjp_entry[1]))
        sgjp_word = sgjp_form[0]
        sgjp_tag = sgjp_form[1]

        tag_match = tagcomp_func(nkjp_tag, sgjp_tag) # do it once

        if sgjp_word == nkjp_word and tag_match: # word_nkjp_word & tag
            found = True
            break

        elif tag_match: # tag okay, try with other letter cases
            if len(nkjp_word) > 1 and nkjp_word.lower().capitalize() == nkjp_word: # Aaaa -> aaaa
                if sgjp_word == nkjp_word.lower():
                    case1 = True
            if not case1 and nkjp_word.lower() != nkjp_word:
                if sgjp_word == nkjp_word.capitalize(): # AAAA -> Aaaa
                    case2 = True
                elif sgjp_word == nkjp_word.lower(): # AAAA -> aaaa, A -> a
                    case3 = True
    return (found, case1, case2, case3)

def tab_format(collection, label):
    "Convert a collection used by notmatching() function to a string of tabbed entries."
    fmt = ''
    for etr in collection:
        fmt = fmt + '\t'.join(etr)+ '\t' + label + '\n'
    #print("formatted for "+label+", "+str(len(fmt)) + " bytes")
    return fmt

def nonalphab(string):
    for char in string:
        if unicodedata.category(char)[0] == 'L': # 'letter'
            return False
    return True

def notmatching(nkjp, sgjp, tagcomp_func, result_file):
    notmatching = []
    matching = []
    case1_notmatching = [] # Aaaa -> aaaa
    case2_notmatching = [] # AAAA -> Aaaa
    case3_notmatching = [] # AAAA -> aaaa, A -> a
    lower_matching = [] # matching with form and lemma converted to lowercase
    symbols = []
    notmatching_numeric = []

    for (n, nkjp_entry) in enumerate(nkjp):

        lemma = nkjp_entry[1].strip()
        form = nkjp_entry[0].strip()
        # Warn about stripped whitespaces.
        if lemma != nkjp_entry[1]:
            print("Stripped whitespaces in lemma: %s" % nkjp_entry[1])
        if form != nkjp_entry[0]:
            print("Stripped whitespaces in form: %s" % nkjp_entry[0])

        # Abbreviations are automatically classified as symbols.
        if nkjp_entry[2][:4] == 'brev':
            symbols.append(nkjp_entry)
            continue

        sgjp_forms = []
        lowered_lemma = False # indicates if lemma was converted to lowercase
        if lemma in sgjp: # lemma matching
            sgjp_forms = sgjp[lemma]
        else:
            if lemma.lower() in sgjp:
                lowered_lemma = True
                sgjp_forms = sgjp[lemma.lower()]
            else:
                # Continue when we can't find even lowered lemma in SGJP. 
                if nonalphab(form) and nonalphab(lemma):
                    symbols.append(nkjp_entry)
                elif re.match(r"^[123456789]", form, flags=re.L) != None:
                    notmatching_numeric.append(nkjp_entry)
                    continue
                else:
                    notmatching.append(nkjp_entry)
                continue

        # The following is executed only if the lemma (maybe in lowercase) was found in SGJP.

        # Go through the entry if it wasn't found in SGJP
        found, case1, case2, case3 = 0, 1, 2, 3 # indices in boolean tuple below
        case = compare_entries(nkjp_entry, sgjp_forms, tagcomp_func)

        # one more desperate attempt at lowering the lemma, if nothing was found
        if (not lowered_lemma) and not True in case:
            if lemma.lower() in sgjp:
                sgjp_forms = sgjp[lemma.lower()]
                case = compare_entries(nkjp_entry, sgjp_forms, tagcomp_func)
                if True in case:
                    lowered_lemma = True
                else: # revert for consistency 
                    sgjp_forms = sgjp[lemma]

        if lowered_lemma and (case[found] or case[case1] or case[case2] or case[case3]):
            lower_matching.append(nkjp_entry)
            continue

        if case[found]:
            matching.append(nkjp_entry)
            continue

        if nonalphab(form) and nonalphab(lemma):
            symbols.append(nkjp_entry)
            continue
        if re.match(r"^[123456789]", form, flags=re.L) != None:
            notmatching_numeric.append(nkjp_entry)
            continue

        if case[case1]:
            case1_notmatching.append(nkjp_entry)
            continue
        if case[case2]:
            case2_notmatching.append(nkjp_entry)
            continue
        if case[case3]:
            case3_notmatching.append(nkjp_entry)
            continue

        # when everything failed:
        notmatching.append(nkjp_entry)

    collections = [nkjp, matching, case1_notmatching, case2_notmatching, case3_notmatching,
                lower_matching, symbols, notmatching_numeric, notmatching]
    # sort the entries in collections by frequency
    collections = list(map((lambda coll: sorted(coll, reverse=True, key=(lambda etr: int(etr[3])))),
                        collections))
    freqs = list(map(lambda coll: functools.reduce((lambda x, y: x+y),
        [int(etr[3]) for etr in coll]), # sum of sets' frequencies
                    collections))
    descs = ["Total:",
            "Found:",
            "Found when Aaa -> aaa (lemma):",
            "Found when AAA -> Aaa (lemma):",
            "Found when AAA -> aaa (lemma):",
            "Found when word form and lemma are converted to lowercase:",
            "Symbols:",
            "Not found, numeric:",
            "Not found, other:"]

    for (i, _) in enumerate(collections):
        info = (len(collections[i]), 100.0*(len(collections[i])/len(collections[0])),
                 freqs[i], 100.0*(freqs[i]/freqs[0]))
        print((descs[i]+" %d entries (%.2f%%), %d occurences (%.2f%%)") % info)
    
    # below we skip nkjp, which contains everything
    labels = ['SGJP-EXACT\tNCH\tCORR', 'SGJP-LMM-UNCAPITAL\tNCH\tCORR',
            'SGJP-LMM-CAPITAL\tNCH\tCORR', 'SGJP-LMM-LOWER\tNCH\tCORR',
            'SGJP-BTH-LOWER\tNCH\tCORR', 'NON-SGJP\tSYMB\tCORR',
            'NON-SGJP\tLATEK\tCORR', 'NON-SGJP\tCW\tCORR']
    with open(result_file, 'w+') as out:
        for (c, coll) in enumerate(collections[1:]):
            print(tab_format(coll, labels[c]), file=out)
