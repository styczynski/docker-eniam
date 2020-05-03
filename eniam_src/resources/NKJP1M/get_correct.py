# This script prints to stdout a version of frequency list with all manual
# corrections. Tokenization errors are ignored (not printed), as of now.
corrections = {}
amend_CERR = False # whether to enter the corrections for CERR's (common errors)
with open('NKJP1M-frequency-with-corrections.tab') as corr:
    for line in corr:
        entry = line.strip().split('\t')
        # a triple: original form, lemma, interpretation
        corrections[(entry[0], entry[1], entry[3])] = entry

with open('NKJP1M-tagged-frequency-30.06.2017.tab') as freq:
    for line in freq:
        entry = line.strip().split('\t')
        if (entry[-1] == 'ERR' or (amend_CERR and entry[-1] == 'CERR')) and tuple(entry[:3]) in corrections:
            correction = corrections[tuple(entry[:3])]
            if len(correction) == 9:
                entry[0] = correction[2]
                entry[-1] = 'CORR'
                print('\t'.join(entry))
            else: # TERR's
                continue # ignore, don't print
        elif entry[-1] in ['TAGD', 'TAGE', 'PLTAN', 'PHON', 'ERR-TAGE', 'ERR-TAGD', 'ERR-PLTAN', 'CERR-TAGE', 'CERR-TAGD', 'CERR-PLTAN', 'ERR', 'CERR']:
            continue
        else:
            print('\t'.join(entry))
