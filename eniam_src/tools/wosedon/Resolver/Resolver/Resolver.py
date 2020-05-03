import argparse
import os
import xml.etree.ElementTree as ET


IN_SENTENCE = 'in_sentence'
IN_PARAGRAPH = 'in_paragraph'
IN_DOCUMENT = 'in_document'


def make_parser():
    desc = 'Extracts graph of synsets from ccl files.'
    parser = argparse.ArgumentParser(description = desc)
    parser.add_argument('-f', '--file-list', help='file containing pathes of files to process, relative to both input_dir and input_dir2', required=True)
    parser.add_argument('-i', '--input-dir', help='directory where files with primary rankings are stored', required=True)
    parser.add_argument('-j', '--input-dir2', help='directory where files with secondary rankings are stored', required=True)
    parser.add_argument('-o', '--output-dir', help='directory whither results should be stored', required=True)
    parser.add_argument('-d', '--diff', help='difference from the best to be considered for replacing it, as float', type=float, default=0.3)
    return parser


def make_pair(txt):
    a, b = txt.split('/')
    return int(a), float(b)


def to_resolve(txt, diff):
    txt = txt.split(' ')
    res = set()
    best = make_pair(txt[0])
    for pair in txt:
        pair = make_pair(pair)
        if abs(pair[1] - best[1]) / best[1] < diff:
            res.add(pair[0])
    return res


def help(helper, worthy_synsets):
    res = []
    for prop in helper.findall('prop'):
        if prop.get('key') == 'sense:ukb:syns_rank':
            txt = prop.text.split(' ')
            for pair in txt:
                pair = make_pair(pair)
                if pair[0] in worthy_synsets:
                    res.append(pair)
            return res


def resolve(node, helper, diff):
    for prop in node.findall('prop'):
        if prop.get('key') == 'sense:ukb:syns_rank':
            tr = to_resolve(prop.text, diff)
            if len(tr) > 1:
                newranking = help(helper, tr)
                prop.text = " ".join([str(a) + '/' + str(b) for a,b in newranking])
                for prop2 in node.findall('prop'):
                    if prop2.get('key') == 'sense:ukb:syns_id':
                        prop2.text = str(newranking[0][0])
                        break
                for prop2 in node.findall('prop'):
                    if prop2.get('key') == 'sense:ukb:unitsstr':
                      #node.remove(prop2)
                      prop2.text = ''
                      break
            break
        
  

def main(argv = None):
    parser = make_parser()
    args = parser.parse_args(argv)
    
    file_list = []
    with open(args.file_list) as f:
      for l in f:
        file_list.append(l.strip())
    
    counter = 0
    for path in file_list:
        counter += 1
        print "Processing", path, "(" + str(counter) + "/" + str(len(file_list)) + " = " + str(round(counter * 100.0 / len(file_list), 3)) + "%)"
        
        xml = ET.parse(os.path.join(args.input_dir, path))
        xml2 = ET.parse(os.path.join(args.input_dir2, path))
        
        root = xml.getroot()
        root2 = xml2.getroot()
        
        for chnr in range(len(root)):
            for senr in range(len(root[chnr])):
                for tonr in range(len(root[chnr][senr])):
                    resolve(root[chnr][senr][tonr], root2[chnr][senr][tonr], args.diff)
        
        filename = os.path.join(args.output_dir, path)
        
        if not os.path.exists(os.path.dirname(filename)):
            os.makedirs(os.path.dirname(filename))
        
        xml.write(filename, encoding="utf-8", xml_declaration=True)
        
        
        
                            
                        


if __name__ == '__main__':
        main()
