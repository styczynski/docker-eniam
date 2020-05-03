# -*- coding: UTF-8 -*-
# Blame Szymon Rutkowski - June 2017

from bs4 import BeautifulSoup
from subprocess import call

source = 'NKJP1M-corrected-5.07.2017.tab' # the original frequency list
output = 'freq_with_rules2.tab' # here the list with new COMPOS classification will be written
cleanup = True # whether we should remove temporary files at the end

forms = []
with open(source) as freq:
    for line in freq:
        forms.append(line.strip().split('\t')[0])

form_interps = {} # form -> list of lemmas # (lemma, interp)

def fill_interps():
    # run ENIAM to get interps
    with open('temp-forms', 'w+') as tempout:
        print('\n\n'.join(forms_batch), file=tempout)
    call('cat temp-forms | ../../subsyntax/subsyntax -n -x > temp-xml', shell=True)

    # process the ENIAM output
    with open('temp-xml') as inp:
        soup = BeautifulSoup(inp, 'lxml')
        tokens = soup.find_all('token_record')
        for token in tokens:
            this_form = token.find('orth').string
            this_interps = []

            # Collect interps
            for proper in token.find_all('proper'):
                #interp = proper['pos']
                #tag = proper.find('interp').string
                #if tag:
                #    interp += ':' + tag
                this_interps.append(proper['lemma'])
            for lemma in token.find_all('lemma'):
                #interp = lemma['pos']
                #tag = lemma.find('interp').string
                #if tag:
                #    interp += ':' + tag
                this_interps.append(lemma['lemma'])

            # Write interps to the dictionary
            if this_form in form_interps:
                form_interps[this_form] += this_interps
            else:
                form_interps[this_form] = this_interps

counter = 0
forms_batch = []
with open('temp-compos', 'w+') as out:
    for form in forms:
        forms_batch.append(form)
        counter += 1
        if counter == 5000:
            fill_interps()
            counter = 0
            forms_batch = []

            # print interpretations and reset
            for form_i in form_interps:
                    print('{}\t{}'.format(form_i, list(set(form_interps[form_i]))), file=out)
            form_interps = {}

    # for the remaining, last chunk of entries:
    fill_interps()
    for form_i in form_interps:
        print('{}\t{}'.format(form_i, list(set(form_interps[form_i]))), file=out)
    counter = 0
    form_interps = {}

# Load the lemmas again
with open('temp-compos') as compos_inp:
    compos_content = compos_inp.read().split('\n')
    for line in compos_content:
        entry = line.strip().split('\t')
        if len(entry) == 1:
            continue
        # Update the form -> lemmas mapping.
        # (remove quotation marks and commas from the lists that were written)
        if entry[0].lower() in form_interps:
            form_interps[entry[0].lower()] += [interp[1:-1].lower() for interp in entry[1][1:-1].split(", ")]
        else:
            form_interps[entry[0].lower()] = [interp[1:-1].lower() for interp in entry[1][1:-1].split(", ")]

def esccurl(string) :
    "Escape the curly brackets in the string, for using it with the string formatter."
    return string.replace('{', '{{').replace('}', '}}')

# Finally, determine compositionalities
with open(source) as inp:
    with open(output, 'w+') as out:
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

            # look up the entry form, check if the lemma is among possibilities
            if data[0].lower() in form_interps and data[1].lower() in form_interps[data[0].lower()]:
                print(fmt.format('COMPOS'), file=out)
            # pass if the entry lemma is the same as its form
            elif data[0].lower() == data[1].lower():
                print(fmt.format('COMPOS-ndm'), file=out)
            else:
                print(fmt.format('NCOMPOS'), file=out)

if cleanup:
    call(['rm', 'temp-forms', 'temp-xml', 'temp-compos'])
