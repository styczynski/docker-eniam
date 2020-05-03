PREC_GEN_DESCRIPTION ='''
Podsumowauje plik *prec i wypisuje ogolna informacje 
o precyzji dla rzeczownikow, czasownikow i srednia z dwoch.
'''

import argparse

def make_parser(desc):
    aparser = argparse.ArgumentParser(description = desc)
    aparser.add_argument('-p', '--precision-filename', 
            dest = 'precision_filename', required = True)
    return aparser

def make_general_stats(prec_filename):
    v_correct = 0
    v_incorrect = 0
    n_correct = 0
    n_incorrect = 0
    
    with open(prec_filename, 'rt') as fin:
        fin.next()
        for line in fin:
            line = line.strip()
            spl_line = line.split(';')
            corr = float(spl_line[5])
            incorr = float(spl_line[4])
            if spl_line[1] == 'n':
                n_correct += corr
                n_incorrect += incorr
            if spl_line[1] == 'v':
                v_correct += corr
                v_incorrect += incorr
    n_sum = (n_correct + n_incorrect)
    n_sum += 0 if n_sum else 1
    
    v_sum = (v_correct + v_incorrect)
    v_sum += 0 if v_sum else 1
    
    n = n_correct / n_sum
    v = v_correct / v_sum
    a = (n_correct + v_correct) / ((n_correct + n_incorrect) + (v_correct + v_incorrect))
    print 'V;', 'Correct;', v_correct, ';Incorrect;', v_incorrect, ';Precision;', v
    print 'N;', 'Correct;', n_correct, ';Incorrect;', n_incorrect, ';Precision;', n
    print '_;', \
            'Correct;', (v_correct+ n_correct), \
            ';Incorrect;', (v_incorrect + n_incorrect), \
            ';Precision;', a
    return [100 * n, 100 * v, 100 * a]

def main(argv = None):
    parser = make_parser(PREC_GEN_DESCRIPTION)
    args = parser.parse_args(argv)

    n, v, d = make_general_stats(args.precision_filename)


if __name__ == '__main__':
    main()
